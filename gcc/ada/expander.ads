------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P A N D E R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This procedure performs any required expansion for the specified node.
--  The argument is the node that is a candidate for possible expansion.
--  If no expansion is required, then Expand returns without doing anything.

--  If the node does need expansion, then the subtree is replaced by the
--  tree corresponding to the required rewriting. This tree is a syntactic
--  tree, except that all Entity fields must be correctly set on all
--  direct names, since the expander presumably knows what it wants, and in
--  any case it doesn't work to have the semantic analyzer perform visibility
--  analysis on these trees (they may have references to non-visible runtime
--  routines etc.) There are a few exceptions to this rule in special cases,
--  but they must be documented clearly.

--  Expand is called in two different situations:

--    Nodes that are not subexpressions (Nkind not in N_Subexpr)

--      In this case, Expand is called from the body of Sem, immediately
--      after completing semantic analysis by calling the corresponding
--      Analyze_N_xxx procedure. If expansion occurs, the given node must
--      be replaced with another node that is also not a subexpression.
--      This seems naturally to be the case, since it is hard to imagine any
--      situation in which it would make sense to replace a non-expression
--      subtree with an expression. Once the substitution is completed, the
--      Expand routine must call Analyze on the resulting node to do any
--      required semantic analysis. Note that references to children copied
--      from the old tree won't be reanalyzed, since their Analyze flag is set.

--    Nodes that are subexpressions (Nkind in N_Subexpr)

--      In this case, Expand is called from Sem_Res.Resolve after completing
--      the resolution of the subexpression (this means that the expander sees
--      the fully typed subtree). If expansion occurs, the given node must be
--      replaced by a node that is also a subexpression. Again it is hard
--      to see how this restriction could possibly be violated. Once the
--      substitution is completed, the Expand routine must first call Analyze
--      on the resulting node to do any required semantic analysis, and then
--      call Resolve on the node to set the type (typically the type will be
--      the same as the original type of the input node, but this is not
--      always the case).

--  In both these cases, Replace or Rewrite must be used to achieve the
--  of the node, since the Expander routine is only passed the Node_Id
--  of the node to be expanded, and the resulting expanded Node_Id must
--  be the same (the parameter to Expand is mode in, not mode in-out).

--  For nodes other than subexpressions, it is not necessary to preserve the
--  original tree in the Expand routines, unlike the case for modifications
--  to the tree made in the semantic analyzer. This is because anyone who is
--  interested in working with the original tree (like ASIS) is required to
--  compile in semantics checks only mode. Thus Replace may be freely used
--  in such instances.

--  For subexpressions, preservation of the original tree is required because
--  of the need for conformance checking of default expressions, which occurs
--  on expanded trees. This means that Replace should not ever be used on
--  on subexpression nodes. Instead use Rewrite.

--  Note: the front end avoids calls to any of the expand routines if code
--  is not being generated. This is done for three reasons:

--    1.  Make sure tree does not get mucked up by the expander if no
--        code is being generated, and is thus usable by ASIS etc.

--    2.  Save time, since expansion is not needed if a compilation is
--        being done only to check the semantics, or if code generation
--        has been canceled due to previously detected errors.

--    3.  Allow the expand routines to assume that the tree is error free.
--        This results from the fact that code generation mode is always
--        cancelled when any error occurs.

--  If we ever decide to implement a feature allowing object modules to be
--  generated even if errors have been detected, then point 3 will no longer
--  hold, and the expand routines will have to be modified to operate properly
--  in the presence of errors (for many reasons this is not currently true).

--  Note: a consequence of this approach is that error messages must never
--  be generated in the expander, since this would mean that such error
--  messages are not generated when the expander is not being called.

--  Expansion is the last stage of analyzing a node, so Expand sets the
--  Analyzed flag of the node being analyzed as its last action. This is
--  done even if expansion is off (in this case, the only effect of the
--  call to Expand is to set the Analyzed flag to True).

with Types; use Types;

package Expander is

   --  The flag Opt.Expander_Active controls whether expansion is active
   --  (True) or deactivated (False). When expansion is deactivated all
   --  calls to expander routines have no effect. To temporarily disable
   --  expansion, always call the routines defined below, do NOT change
   --  Expander_Active directly.
   --
   --  You should not use this flag to test if you are currently processing
   --  a generic spec or body. Use the flag Inside_A_Generic instead (see
   --  the spec of package Sem).
   --
   --  There is no good reason for permanently changing the value of this flag
   --  except after detecting a syntactic or semantic error. In this event
   --  this flag is set to False to disable all subsequent expansion activity.
   --
   --  In general this flag should be used as a read only value. The only
   --  exceptions where it makes sense to temporarily change its value are:
   --
   --    (a) when starting/completing the processing of a generic definition
   --        or declaration (see routines Start_Generic_Processing and
   --        End_Generic_Processing in Sem_Ch12)
   --
   --    (b) when starting/completing the pre-analysis of an expression
   --        (see the spec of package Sem for more info on pre-analysis.)
   --
   --  Note that when processing a default expression (In_Default_Expression
   --  is True) or performing semantic analysis of a generic spec or body
   --  (Inside_A_Generic) or when performing pre-analysis (Full_Analysis is
   --  False) the Expander_Active flag is False.

   procedure Expand (N : Node_Id);
   --  Expand node N, as described above

   procedure Expander_Mode_Save_And_Set (Status : Boolean);
   --  Saves the current setting of the Expander_Active flag on an internal
   --  stack and then sets the flag to the given value.

   procedure Expander_Mode_Restore;
   --  Restores the setting of the Expander_Active flag using the top entry
   --  pushed onto the stack by Expander_Mode_Save_And_Reset, popping the
   --  stack, except that if any errors have been detected, then the state
   --  of the flag is left set to False.

end Expander;
