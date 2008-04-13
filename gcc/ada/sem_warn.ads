------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ W A R N                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines used to deal with issuing warnings
--  about uses of uninitialized variables and unused with's. It also has
--  some unrelated routines related to the generation of warnings.

with Alloc; use Alloc;
with Table;
with Types; use Types;

package Sem_Warn is

   ------------------------
   -- Warnings Off Table --
   ------------------------

   type Warnings_Off_Entry is record
      N : Node_Id;
      --  A pragma Warnings (Off, ent) node

      E : Entity_Id;
      --  The entity involved
   end record;

   --  An entry is made in the following table for any valid Pragma Warnings
   --  (Off, entity) encountered while Opt.Warn_On_Warnings_Off is True. It
   --  is used to generate warnings on any of these pragmas that turn out not
   --  to be needed, or that could be replaced by Unmodified/Unreferenced.

   package Warnings_Off_Pragmas is new Table.Table (
     Table_Component_Type => Warnings_Off_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Warnings_Off_Pragmas_Initial,
     Table_Increment      => Alloc.Warnings_Off_Pragmas_Increment,
     Table_Name           => "Name_Warnings_Off_Pragmas");

   --------------------
   -- Initialization --
   --------------------

   procedure Initialize;
   --  Initialize this package for new compilation

   function Set_Warning_Switch (C : Character) return Boolean;
   --  This function sets the warning switch or switches corresponding to the
   --  given character. It is used to process a -gnatw switch on the command
   --  line, or a character in a string literal in pragma Warnings. Returns
   --  True for valid warning character C, False for invalid character.

   function Set_Dot_Warning_Switch (C : Character) return Boolean;
   --  This function sets the warning switch or switches corresponding to the
   --  given character preceded by a dot. Used to process a -gnatw. switch on
   --  the command line or .C in a string literal in pragma Warnings. Returns
   --  True for valid warning character C, False for invalid character.

   ------------------------------------------
   -- Routines to Handle Unused References --
   ------------------------------------------

   procedure Check_References (E : Entity_Id; Anod : Node_Id := Empty);
   --  Called at the end of processing a declarative region. The entity E
   --  is the entity for the scope. All entities declared in the region,
   --  as indicated by First_Entity and the entity chain, are checked to
   --  see if they are variables for which warnings need to be posted for
   --  either no assignments, or a use before an assignment or no references
   --  at all. The Anod node is present for the case of an accept statement,
   --  and references the accept statement. This is used to place the warning
   --  messages in the right place.

   procedure Check_Unset_Reference (N : Node_Id);
   --  N is the node for an expression which occurs in a reference position,
   --  e.g. as the right side of an assignment. This procedure checks to see
   --  if the node is a reference to a variable entity where the entity has
   --  Not_Assigned set. If so, the Unset_Reference field is set if it is not
   --  the first occurrence. No warning is posted, instead warnings will be
   --  posted later by Check_References. The reason we do things that
   --  way is that if there are no assignments anywhere, we prefer to flag
   --  the entity, rather than a reference to it. Note that for the purposes
   --  of this routine, a type conversion or qualified expression whose
   --  expression is an entity is also processed. The reason that we do not
   --  process these at the point of occurrence is that both these constructs
   --  can occur in non-reference positions (e.g. as out parameters).

   procedure Check_Unused_Withs (Spec_Unit : Unit_Number_Type := No_Unit);
   --  This routine performs two kinds of checks. It checks that all with'ed
   --  units are referenced, and that at least one entity of each with'ed
   --  unit is referenced (the latter check catches units that are only
   --  referenced in a use or package renaming statement). Appropriate
   --  warning messages are generated if either of these situations is
   --  detected.
   --
   --  A special case arises when a package body or a subprogram body with
   --  a separate spec is being compiled. In this case, a with may appear
   --  on the spec, but be needed only by the body. This still generates
   --  a warning, but the text is different (the with is not redundant,
   --  it is misplaced).
   --
   --  This special case is implemented by making an initial call to this
   --  procedure with Spec_Unit set to the unit number of the separate spec.
   --  This call does not generate any warning messages, but instead may
   --  result in flags being set in the N_With_Clause node that record that
   --  there was no use in the spec.
   --
   --  The main call (made after all units have been analyzed, with Spec_Unit
   --  set to the default value of No_Unit) generates the required warnings
   --  using the flags set by the initial call where appropriate to specialize
   --  the text of the warning messages.

   ---------------------
   -- Output Routines --
   ---------------------

   procedure Output_Non_Modifed_In_Out_Warnings;
   --  Warnings about IN OUT parameters that could be IN are collected till
   --  the end of the compilation process (see body of this routine for a
   --  discussion of why this is done). This procedure outputs the warnings.
   --  Note: this should be called before Output_Unreferenced_Messages, since
   --  if we have an IN OUT warning, that's the one we want to see!

   procedure Output_Obsolescent_Entity_Warnings (N : Node_Id; E : Entity_Id);
   --  N is a reference to obsolescent entity E, for which appropriate warning
   --  messages are to be generated (caller has already checked that warnings
   --  are active and appropriate for this entity).

   procedure Output_Unreferenced_Messages;
   --  Warnings about unreferenced entities are collected till the end of
   --  the compilation process (see Check_Unset_Reference for further
   --  details). This procedure outputs waiting warnings, if any.

   procedure Output_Unused_Warnings_Off_Warnings;
   --  Warnings about pragma Warnings (Off, ent) statements that are unused,
   --  or could be replaced by Unmodified/Unreferenced pragmas, are collected
   --  till the end of the compilation process. This procedure outputs waiting
   --  warnings if any.

   ----------------------------
   -- Other Warning Routines --
   ----------------------------

   procedure Check_Code_Statement (N : Node_Id);
   --  Perform warning checks on a code statement node

   procedure Check_Infinite_Loop_Warning (Loop_Statement : Node_Id);
   --  N is the node for a loop statement. This procedure checks if a warning
   --  should be given for a possible infinite loop, and if so issues it.

   procedure Warn_On_Known_Condition (C : Node_Id);
   --  C is a node for a boolean expression resulting from a relational
   --  or membership operation. If the expression has a compile time known
   --  value, then a warning is output if all the following conditions hold:
   --
   --    1. Original expression comes from source. We don't want to generate
   --       warnings for internally generated conditionals.
   --
   --    2. As noted above, the expression is a relational or membership
   --       test, we don't want to generate warnings for boolean variables
   --       since this is typical of conditional compilation in Ada.
   --
   --    3. The expression appears in a statement, rather than a declaration.
   --       In practice, most occurrences in declarations are legitimate
   --       conditionalizations, but occurrences in statements are often
   --       errors for which the warning is useful.
   --
   --    4. The expression does not occur within an instantiation. A non-
   --       static expression in a generic may become constant because of
   --       the attributes of the actuals, and we do not want to warn on
   --       these legitimate constant foldings.
   --
   --  If all these conditions are met, the warning is issued noting that
   --  the result of the test is always false or always true as appropriate.

   function Warn_On_Modified_As_Out_Parameter (E : Entity_Id) return Boolean;
   --  Returns True if we should activate warnings for entity E being modified
   --  as an out parameter. True if either Warn_On_Modified_Unread is set for
   --  an only OUT parameter, or if Warn_On_All_Unread_Out_Parameters is set.

   procedure Warn_On_Suspicious_Index (Name : Entity_Id; X : Node_Id);
   --  This is called after resolving an indexed component or a slice. Name
   --  is the entity for the name of the indexed array, and X is the subscript
   --  for the indexed component case, or one of the bounds in the slice case.
   --  If Name is an unconstrained parameter of a standard string type, and
   --  the index is of the form of a literal or Name'Length [- literal], then
   --  a warning is generated that the subscripting operation is possibly
   --  incorrectly assuming a lower bound of 1.

   procedure Warn_On_Unassigned_Out_Parameter
     (Return_Node : Node_Id;
      Scope_Id    : Entity_Id);
   --  Called when processing a return statement given by Return_Node. Scope_Id
   --  is the Entity_Id for the procedure in which the return statement lives.
   --  A check is made for the case of a procedure with out parameters that
   --  have not yet been assigned, and appropriate warnings are given.

   procedure Warn_On_Useless_Assignment
     (Ent : Entity_Id;
      N   : Node_Id := Empty);
   --  Called to check if we have a case of a useless assignment to the given
   --  entity Ent, as indicated by a non-empty Last_Assignment field. This call
   --  should only be made if at least one of the flags Warn_On_Modified_Unread
   --  or Warn_On_All_Unread_Out_Parameters is True, and if Ent is in the
   --  extended main source unit. N is Empty for the end of block call
   --  (warning message says value unreferenced), or the it is the node for
   --  an overwriting assignment (warning message points to this assignment).

   procedure Warn_On_Useless_Assignments (E : Entity_Id);
   pragma Inline (Warn_On_Useless_Assignments);
   --  Called at the end of a block or subprogram. Scans the entities of the
   --  block or subprogram to see if there are any variables for which useless
   --  assignments were made (assignments whose values were never read).

end Sem_Warn;
