------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        A C C E S S I B I L I T Y                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2022-2026, Free Software Foundation, Inc.         --
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

--  Accessibility level calculation and check generation routines

with Types; use Types;
with Uintp; use Uintp;

package Accessibility is

   procedure Accessibility_Message (N : Node_Id; Typ : Entity_Id);
   --  Error, or warning within an instance, if the static accessibility
   --  rules of 3.10.2 are violated.

   procedure Apply_Accessibility_Check_For_Class_Wide_Allocator
     (N              : Node_Id;
      Exp            : Node_Id;
      Ref            : Node_Id;
      Built_In_Place : Boolean := False);
   --  Ada 2005 (AI-344): For an allocator with a class-wide designated
   --  type, generate an accessibility check to verify that the level of the
   --  type of the created object is not deeper than the level of the access
   --  type. If the type of the qualified expression is class-wide, then
   --  always generate the check (except in the case where it is known to be
   --  unnecessary, see comment below). Otherwise, only generate the check
   --  if the level of the qualified expression type is statically deeper
   --  than the access type.
   --
   --  Although the static accessibility will generally have been performed
   --  as a legality check, it won't have been done in cases where the
   --  allocator appears in generic body, so a run-time check is needed in
   --  general. One special case is when the access type is declared in the
   --  same scope as the class-wide allocator, in which case the check can
   --  never fail, so it need not be generated.
   --
   --  As an open issue, there seem to be cases where the static level
   --  associated with the class-wide object's underlying type is not
   --  sufficient to perform the proper accessibility check, such as for
   --  allocators in nested subprograms or accept statements initialized by
   --  class-wide formals when the actual originates outside at a deeper
   --  static level. The nested subprogram case might require passing
   --  accessibility levels along with class-wide parameters, and the task
   --  case seems to be an actual gap in the language rules that needs to
   --  be fixed by the ARG. ???

   procedure Apply_Accessibility_Check_For_Conversion
     (N           : Node_Id;
      Typ         : Entity_Id;
      Insert_Node : Node_Id);
   --  Given a name N denoting an access parameter or an SAOOAAAT, insert a
   --  run-time check that the accessibility level of the anonymous access
   --  type of the object is not deeper than the level of Typ. Insert_Node
   --  indicates the node where the check should be inserted.

   procedure Apply_Accessibility_Check_For_Discriminated_Allocator
     (N : Node_Id);
   --  If the subtype determined by the subtype indication or the qualified
   --  expression of an allocator N has access discriminants, insert checks
   --  that the accessibility level of each entity designated by an access
   --  discriminant is not deeper than the level of the allocator.

   procedure Apply_Accessibility_Check_For_Return
     (Exp  : Node_Id;
      Func : Entity_Id);
   --  Insert the required run-time accessibility checks for an expression Exp
   --  that is being returned from function Func.

   procedure Check_Return_Construct_Accessibility
     (Return_Stmt : Node_Id;
      Stm_Entity  : Entity_Id);
   --  Apply legality rules of 6.5(5.9) and 6.8(5) to the access discriminants
   --  of an identifier or aggregate in a return statement.

   function Dynamic_Accessibility_Level
     (Expr              : Node_Id;
      In_Return_Context : Boolean := False;
      Allow_Alt_Model   : Boolean := True) return Node_Id;
   --  Return the dynamic accessibility level of a given expression Expr, i.e.
   --  the accessibility level that is defined by the language in RM 3.10.2(3)
   --  as a value "reflect[ing] the run-time nesting of masters".

   --  In_Return_Context forces the level calculation to be carried out as if
   --  Expr was an operative constituent of a return value; when it is False,
   --  the function computes whether that is the case or not.

   --  The Allow_Alt_Model parameter allows the alternative level calculation
   --  under the restriction No_Dynamic_Accessibility_Checks to be performed.

   function Dynamic_Local_Access_Level (E : Entity_Id) return Node_Id;
   --  Return the dynamic accessibility level of the declaration of E. This is
   --  the scope depth of the enclosing dynamic scope of E, offsetted by the
   --  extra accessibility level of the enclosing subprogram of E if it exists.

   function Dynamic_Subprogram_Access_Level (Subp : Entity_Id) return Node_Id;
   --  Return the dynamic accessibility level of Subp, which is the level of
   --  the innermost master of the declaration of Subp (modulo renaming) and,
   --  in particular, is *not* the level of the entities declared within Subp.
   --  It is used to enforce the accessibility rules for access-to-subprogram
   --  types, results of function calls, and formal objects of generic units.

   function Dynamic_Type_Access_Level
     (Typ             : Entity_Id;
      Deepest         : Boolean := False;
      Allow_Alt_Model : Boolean := True) return Node_Id;
   --  Return the dynamic accessibility level of Typ

   --  When Deepest is True, and Typ is that of an Ada 2012 stand-alone object
   --  of an anonymous access type, then return the dynamic accessibility level
   --  of the declaration of the object instead of the library level; moreover,
   --  in the case of a descendant of a generic formal type, return Int'Last
   --  instead of the library level.

   --  The Allow_Alt_Model parameter allows the alternative level calculation
   --  under the restriction No_Dynamic_Accessibility_Checks to be performed.

   function Extra_Accessibility (Id : Entity_Id) return Entity_Id;
   --  Same as Extra_Accessibility_Of_Object in Einfo, but looks through object
   --  renamings per the RM 3.10.2(8) rule.

   function Has_Access_Values (T : Entity_Id) return Boolean;
   --  Returns true if the underlying type of T is an access type, or has a
   --  component (at any recursive level) that is an access type. This is a
   --  conservative predicate, if it is not known whether or not T contains
   --  access values (happens for generic formals in some cases), then False is
   --  returned.  Note that tagged types return False. Even though the tag is
   --  implemented as an access type internally, this function tests only for
   --  access types known to the programmer. See also Has_Tagged_Component.

   function Has_Anonymous_Access_Discriminant (Typ : Entity_Id) return Boolean;
   --  Returns True if Typ has one or more anonymous access discriminants

   function Has_Unconstrained_Access_Discriminants
     (Subtyp : Entity_Id) return Boolean;
   --  Returns True if the given subtype is unconstrained and has one or more
   --  access discriminants.

   function Needs_Accessibility_Level_Temp_Or_Check
     (Conditional_Expr : Node_Id) return Boolean;
   --  Determine whether a conditional expression occurs in a context that
   --  requires either an associated accessibility-level-valued temp (which
   --  is assigned to in each arm of the conditional expression) or an
   --  accessibility level check (which is pushed down into each arm of the
   --  conditional expression).

   function Needs_Result_Accessibility_Level
     (Func_Id : Entity_Id) return Boolean;
   --  Ada 2012 (AI05-0234): Return True if the function needs an implicit
   --  parameter to identify the accessibility level of the function result
   --  "determined by the point of call". Return False if the type of the
   --  function result is a private type and its completion is unavailable.

   function Prefix_With_Safe_Accessibility_Level
     (N   : Node_Id;
      Typ : Entity_Id) return Boolean;
   --  Return True if the prefix does not have a value conversion of an
   --  array because a value conversion is like an aggregate with respect
   --  to determining accessibility level (RM 3.10.2); even if evaluation
   --  of a value conversion is guaranteed to not create a new object,
   --  accessibility rules are defined as if it might.

   function Static_Accessibility_Level
     (Expr              : Node_Id;
      Object_Decl_Level : Boolean := False;
      In_Return_Context : Boolean := False) return Uint;
   --  Return the static accessibility level of a given expression Expr, i.e.
   --  an integer reflecting the compile-time nesting of master constructs,
   --  for use in static accessibility checks.

   --  If Object_Decl_Level is True, then return the static accessibility level
   --  of the *declaration* of the object in the cases where that of the object
   --  itself is not defined, i.e. access parameters and Ada 2012's stand-alone
   --  objects of an anonymous access type (RM 3.10.2(19.1-2)). If it is False,
   --  then return the library level in these cases.

   --  In_Return_Context forces the level calculation to be carried out as if
   --  Expr was an operative constituent of a return value; when it is False,
   --  the function computes whether that is the case or not.

   function Static_Local_Access_Level (E : Entity_Id) return Uint;
   --  Return the static accessibility level of the declaration of E. This is
   --  the scope depth of the enclosing dynamic scope of E.

   function Static_Subprogram_Access_Level (Subp : Entity_Id) return Uint;
   --  Return the static accessibility level of Subp, which is the level of
   --  the innermost master of the declaration of Subp (modulo renaming) and,
   --  in particular, is *not* the level of the entities declared within Subp.
   --  It is used to enforce the accessibility rules for access-to-subprogram
   --  types, results of function calls, and formal objects of generic units.

   function Static_Type_Access_Level
     (Typ             : Entity_Id;
      Deepest         : Boolean := False;
      Allow_Alt_Model : Boolean := True;
      Assoc_Node      : Node_Id := Empty) return Uint;
   --  Return the static accessibility level of Typ

   --  When Deepest is True, and Typ is that of an Ada 2012 stand-alone object
   --  of an anonymous access type, then return the static accessibility level
   --  of the declaration of the object instead of the library level; moreover,
   --  in the case of a descendant of a generic formal type, return Int'Last
   --  instead of the library level.

   --  The Allow_Alt_Model parameter allows the alternative level calculation
   --  under the restriction No_Dynamic_Accessibility_Checks to be performed.

   --  Assoc_Node allows for the optional specification of a node associated
   --  with Typ. This is used only for anonymous access types where the context
   --  matters in interpreting Typ's level.

end Accessibility;
