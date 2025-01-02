------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains routines involved in tagged types and dynamic
--  dispatching.

with Types; use Types;
package Sem_Disp is

   procedure Check_Controlling_Formals (Typ : Entity_Id; Subp : Entity_Id);
   --  Check that all controlling parameters of Subp are of type Typ, that
   --  defaults for controlling parameters are tag-indeterminate, and that the
   --  nominal subtype of the parameters and result statically match the first
   --  subtype of the controlling type. Issues appropriate error messages if
   --  any of these requirements is not met.

   procedure Check_Dispatching_Call (N : Node_Id);
   --  Check if the call N is a dispatching call. The subprogram is known to be
   --  a dispatching operation. The call is dispatching if all the controlling
   --  actuals are dynamically tagged. This procedure is called after overload
   --  resolution, so the call is known to be unambiguous.

   procedure Check_Dispatching_Operation (Subp, Old_Subp : Entity_Id);
   --  Add Subp to the list of primitive operations of the corresponding type
   --  if it has a parameter of this type and is defined at a proper place for
   --  primitive operations (new primitives are only defined in package spec,
   --  overridden operation can be defined in any scope). If Old_Subp is not
   --  Empty we are in the overriding case. If the tagged type associated with
   --  Subp is a concurrent type (case that occurs when the type is declared
   --  in a generic because the analysis of generics disables generation of the
   --  corresponding record) then this routine does not add Subp to the list of
   --  primitive operations but leaves Subp decorated as dispatching operation
   --  to enable checks associated with the Object.Operation notation.

   procedure Check_Operation_From_Incomplete_Type
     (Subp : Entity_Id;
      Typ  : Entity_Id);
   --  If a primitive subprogram Subp was defined for the incomplete view of
   --  Typ, and the full type declaration is a derived type, then Subp may
   --  override a subprogram inherited from the parent type. In that case,
   --  the inherited subprogram will have been hidden by the current one at
   --  the point of the type derivation, so it does not appear in the list
   --  of primitive operations of the type, and this procedure inserts the
   --  overriding subprogram in the full type's list of primitives by
   --  iterating over the list for the parent type. If instead Subp is a new
   --  primitive, then it's simply appended to the primitive list.

   procedure Check_Operation_From_Private_View (Subp, Old_Subp : Entity_Id);
   --  No action performed if Subp is not an alias of a dispatching operation.
   --  Add Old_Subp (if not already present) to the list of primitives of the
   --  tagged type T of Subp if T is the full view of a private tagged type.
   --  The Alias of Old_Subp is adjusted to point to the inherited procedure
   --  of the full view because it is always this one which has to be called.

   function Covered_Interface_Primitives (Prim : Entity_Id) return Elist_Id;
   --  Returns all the interface primitives covered by Prim, when its
   --  controlling type has progenitors.

   function Covered_Interface_Op (Prim : Entity_Id) return Entity_Id;
   --  Returns the interface primitive that Prim covers, when its controlling
   --  type has progenitors.

   function Find_Controlling_Arg (N : Node_Id) return Node_Id;
   --  Returns the actual controlling argument if N is dynamically tagged, and
   --  Empty if it is not dynamically tagged.

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id;
   --  Check whether the subprogram Subp is dispatching, and find the tagged
   --  type of the controlling argument or arguments. Returns Empty if Subp
   --  is not a dispatching operation.

   function Find_Primitive_Covering_Interface
     (Tagged_Type : Entity_Id;
      Iface_Prim  : Entity_Id) return Entity_Id;
   --  Search the homonym chain for the primitive of Tagged_Type that covers
   --  Iface_Prim. The homonym chain traversal is required to catch primitives
   --  associated with the partial view of private types when processing the
   --  corresponding full view. If the entity is not found, then search for it
   --  in the list of primitives of Tagged_Type. This latter search is needed
   --  when the interface primitive is covered by a private subprogram. If the
   --  primitive has not been covered yet then return the entity that will be
   --  overridden when the primitive is covered (that is, return the entity
   --  whose alias attribute references the interface primitive). If none of
   --  these entities is found then return Empty.

   type Subprogram_List is array (Nat range <>) of Entity_Id;
   --  Type returned by Inherited_Subprograms function

   generic
      with function Find_DT (Subp : Entity_Id) return Entity_Id;
   package Inheritance_Utilities is

      --  This package provides generic versions of inheritance utilities
      --  provided here. These versions are used in GNATprove backend to adapt
      --  these utilities to GNATprove specific version of visibility of types.

      function Inherited_Subprograms
        (S               : Entity_Id;
         No_Interfaces   : Boolean := False;
         Interfaces_Only : Boolean := False;
         Skip_Overridden : Boolean := False;
         One_Only        : Boolean := False) return Subprogram_List;

      function Is_Overriding_Subprogram (E : Entity_Id) return Boolean;
   end Inheritance_Utilities;

   function Inherited_Subprograms
     (S               : Entity_Id;
      No_Interfaces   : Boolean := False;
      Interfaces_Only : Boolean := False;
      Skip_Overridden : Boolean := False;
      One_Only        : Boolean := False) return Subprogram_List;
   --  Given the spec of a subprogram, this function gathers any inherited
   --  subprograms from direct inheritance or via interfaces. The result is an
   --  array of Entity_Ids of the specs of inherited subprograms. Returns a
   --  null array if passed an Empty spec id. Note that the returned array
   --  only includes subprograms and generic subprograms (and excludes any
   --  other inherited entities, in particular enumeration literals). If
   --  No_Interfaces is True, only return inherited subprograms not coming
   --  from an interface. If Interfaces_Only is True, only return inherited
   --  subprograms from interfaces. Otherwise, subprograms inherited directly
   --  come first, starting with the closest ancestors, and are followed by
   --  subprograms inherited from interfaces. At most one of No_Interfaces
   --  and Interfaces_Only should be True.
   --
   --  If Skip_Overridden is True, subprograms overridden by another subprogram
   --  in the result list are skipped.
   --
   --  If One_Only is set, the search is discontinued as soon as one entry
   --  is found. In this case the resulting array is either null or contains
   --  exactly one element.

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean;
   --  Used to determine whether a call is dispatching, i.e. if it is
   --  an expression of a class_Wide type, or a call to a function with
   --  controlling result where at least one operand is dynamically tagged.
   --  Also used to determine whether an entity has a class-wide type, or a
   --  function call that dispatches on the result. Used to verify that all the
   --  dependent expressions in a conditional expression are equally tagged.

   function Is_Null_Interface_Primitive (E : Entity_Id) return Boolean;
   --  Returns True if E is a null procedure that is an interface primitive

   function Is_Overriding_Subprogram (E : Entity_Id) return Boolean;
   --  Returns True if E is an overriding subprogram and False otherwise, in
   --  particular for an inherited subprogram.

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean;
   --  Returns true if the expression N is tag-indeterminate. An expression
   --  is tag-indeterminate if it is a call that dispatches on result, and all
   --  controlling operands are also indeterminate. Such a function call may
   --  inherit a tag from an enclosing call.

   procedure Override_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      Prev_Op     : Entity_Id;
      New_Op      : Entity_Id);
   --  Replace an implicit dispatching operation of the type Tagged_Type
   --  with an explicit one. Prev_Op is an inherited primitive operation which
   --  is overridden by the explicit declaration of New_Op.

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id);
   --  If a function call given by Actual is tag-indeterminate, its controlling
   --  argument is found in the context, given by Control: either from an
   --  operand of an enclosing call, or the left-hand side of the enclosing
   --  assignment statement. The tag of Control will be propagated recursively
   --  to Actual and to its tag-indeterminate operands, if any.

end Sem_Disp;
