------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ A U X                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Package containing utility procedures used throughout the compiler,
--  and also by ASIS so dependencies are limited to ASIS included packages.

--  Historical note. Many of the routines here were originally in Einfo, but
--  Einfo is supposed to be a relatively low level package dealing with the
--  content of entities in the tree, so this package is used for routines that
--  require more than minimal semantic knowledge.

with Alloc; use Alloc;
with Namet; use Namet;
with Table;
with Types; use Types;

package Sem_Aux is

   --------------------------------
   -- Obsolescent Warnings Table --
   --------------------------------

   --  This table records entities for which a pragma Obsolescent with a
   --  message argument has been processed.

   type OWT_Record is record
      Ent : Entity_Id;
      --  The entity to which the pragma applies

      Msg : String_Id;
      --  The string containing the message
   end record;

   package Obsolescent_Warnings is new Table.Table (
     Table_Component_Type => OWT_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Obsolescent_Warnings_Initial,
     Table_Increment      => Alloc.Obsolescent_Warnings_Increment,
     Table_Name           => "Obsolescent_Warnings");

   procedure Initialize;
   --  Called at the start of compilation of each new main source file to
   --  initialize the allocation of the Obsolescent_Warnings table. Note that
   --  Initialize must not be called if Tree_Read is used.

   procedure Tree_Read;
   --  Initializes Obsolescent_Warnings table from current tree file using the
   --  relevant Table.Tree_Read routine.

   procedure Tree_Write;
   --  Writes out Obsolescent_Warnings table to current tree file using the
   --  relevant Table.Tree_Write routine.

   -----------------
   -- Subprograms --
   -----------------

   function Ancestor_Subtype (Typ : Entity_Id) return Entity_Id;
   --  The argument Id is a type or subtype entity. If the argument is a
   --  subtype then it returns the subtype or type from which the subtype was
   --  obtained, otherwise it returns Empty.

   function Available_View (Ent : Entity_Id) return Entity_Id;
   --  Ent denotes an abstract state or a type that may come from a limited
   --  with clause. Return the non-limited view of Ent if there is one or Ent
   --  if this is not the case.

   function Constant_Value (Ent : Entity_Id) return Node_Id;
   --  Ent is a variable, constant, named integer, or named real entity. This
   --  call obtains the initialization expression for the entity. Will return
   --  Empty for a deferred constant whose full view is not available or
   --  in some other cases of internal entities, which cannot be treated as
   --  constants from the point of view of constant folding. Empty is also
   --  returned for variables with no initialization expression.

   function Corresponding_Unsigned_Type (Typ : Entity_Id) return Entity_Id;
   --  Typ is a signed integer subtype. This routine returns the standard
   --  unsigned type with the same Esize as the implementation base type of
   --  Typ, e.g. Long_Integer => Long_Unsigned.

   function Enclosing_Dynamic_Scope (Ent : Entity_Id) return Entity_Id;
   --  For any entity, Ent, returns the closest dynamic scope in which the
   --  entity is declared or Standard_Standard for library-level entities.

   function First_Discriminant (Typ : Entity_Id) return Entity_Id;
   --  Typ is a type with discriminants. The discriminants are the first
   --  entities declared in the type, so normally this is equivalent to
   --  First_Entity. The exception arises for tagged types, where the tag
   --  itself is prepended to the front of the entity chain, so the
   --  First_Discriminant function steps past the tag if it is present.

   function First_Stored_Discriminant (Typ : Entity_Id) return Entity_Id;
   --  Typ is a type with discriminants. Gives the first discriminant stored
   --  in an object of this type. In many cases, these are the same as the
   --  normal visible discriminants for the type, but in the case of renamed
   --  discriminants, this is not always the case.
   --
   --  For tagged types, and untagged types which are root types or derived
   --  types but which do not rename discriminants in their root type, the
   --  stored discriminants are the same as the actual discriminants of the
   --  type, and hence this function is the same as First_Discriminant.
   --
   --  For derived non-tagged types that rename discriminants in the root type
   --  this is the first of the discriminants that occur in the root type. To
   --  be precise, in this case stored discriminants are entities attached to
   --  the entity chain of the derived type which are a copy of the
   --  discriminants of the root type. Furthermore their Is_Completely_Hidden
   --  flag is set since although they are actually stored in the object, they
   --  are not in the set of discriminants that is visible in the type.
   --
   --  For derived untagged types, the set of stored discriminants are the real
   --  discriminants from Gigi's standpoint, i.e. those that will be stored in
   --  actual objects of the type.

   function First_Subtype (Typ : Entity_Id) return Entity_Id;
   --  Applies to all types and subtypes. For types, yields the first subtype
   --  of the type. For subtypes, yields the first subtype of the base type of
   --  the subtype.

   function First_Tag_Component (Typ : Entity_Id) return Entity_Id;
   --  Typ must be a tagged record type. This function returns the Entity for
   --  the first _Tag field in the record type.

   function Get_Rep_Item
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Node_Id;
   --  Searches the Rep_Item chain for a given entity E, for an instance of a
   --  rep item (pragma, attribute definition clause, or aspect specification)
   --  whose name matches the given name Nam. If Check_Parents is False then it
   --  only returns rep item that has been directly specified for E (and not
   --  inherited from its parents, if any). If one is found, it is returned,
   --  otherwise Empty is returned. A special case is that when Nam is
   --  Name_Priority, the call will also find Interrupt_Priority.

   function Get_Rep_Item
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Node_Id;
   --  Searches the Rep_Item chain for a given entity E, for an instance of a
   --  rep item (pragma, attribute definition clause, or aspect specification)
   --  whose name matches one of the given names Nam1 or Nam2. If Check_Parents
   --  is False then it only returns rep item that has been directly specified
   --  for E (and not inherited from its parents, if any). If one is found, it
   --  is returned, otherwise Empty is returned. A special case is that when
   --  one of the given names is Name_Priority, the call will also find
   --  Interrupt_Priority.

   function Get_Rep_Pragma
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Node_Id;
   --  Searches the Rep_Item chain for a given entity E, for an instance of a
   --  representation pragma whose name matches the given name Nam. If
   --  Check_Parents is False then it only returns representation pragma that
   --  has been directly specified for E (and not inherited from its parents,
   --  if any). If one is found and if it is the first rep item in the list
   --  that matches Nam, it is returned, otherwise Empty is returned. A special
   --  case is that when Nam is Name_Priority, the call will also find
   --  Interrupt_Priority.

   function Get_Rep_Pragma
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Node_Id;
   --  Searches the Rep_Item chain for a given entity E, for an instance of a
   --  representation pragma whose name matches one of the given names Nam1 or
   --  Nam2. If Check_Parents is False then it only returns representation
   --  pragma that has been directly specified for E (and not inherited from
   --  its parents, if any). If one is found and if it is the first rep item in
   --  the list that matches one of the given names, it is returned, otherwise
   --  Empty is returned. A special case is that when one of the given names is
   --  Name_Priority, the call will also find Interrupt_Priority.

   function Has_Rep_Item
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Boolean;
   --  Searches the Rep_Item chain for the given entity E, for an instance of a
   --  rep item (pragma, attribute definition clause, or aspect specification)
   --  with the given name Nam. If Check_Parents is False then it only checks
   --  for a rep item that has been directly specified for E (and not inherited
   --  from its parents, if any). If found then True is returned, otherwise
   --  False indicates that no matching entry was found.

   function Has_Rep_Item
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Boolean;
   --  Searches the Rep_Item chain for the given entity E, for an instance of a
   --  rep item (pragma, attribute definition clause, or aspect specification)
   --  with the given names Nam1 or Nam2. If Check_Parents is False then it
   --  only checks for a rep item that has been directly specified for E (and
   --  not inherited from its parents, if any). If found then True is returned,
   --  otherwise False indicates that no matching entry was found.

   function Has_Rep_Pragma
     (E             : Entity_Id;
      Nam           : Name_Id;
      Check_Parents : Boolean := True) return Boolean;
   --  Searches the Rep_Item chain for the given entity E, for an instance of a
   --  representation pragma with the given name Nam. If Check_Parents is False
   --  then it only checks for a representation pragma that has been directly
   --  specified for E (and not inherited from its parents, if any). If found
   --  and if it is the first rep item in the list that matches Nam then True
   --  is returned, otherwise False indicates that no matching entry was found.

   function Has_Rep_Pragma
     (E             : Entity_Id;
      Nam1          : Name_Id;
      Nam2          : Name_Id;
      Check_Parents : Boolean := True) return Boolean;
   --  Searches the Rep_Item chain for the given entity E, for an instance of a
   --  representation pragma with the given names Nam1 or Nam2. If
   --  Check_Parents is False then it only checks for a rep item that has been
   --  directly specified for E (and not inherited from its parents, if any).
   --  If found and if it is the first rep item in the list that matches one of
   --  the given names then True is returned, otherwise False indicates that no
   --  matching entry was found.

   function Has_Unconstrained_Elements (T : Entity_Id) return Boolean;
   --  True if T has discriminants and is unconstrained, or is an array type
   --  whose element type Has_Unconstrained_Elements.

   function In_Generic_Body (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id appears inside a generic body

   function Initialization_Suppressed (Typ : Entity_Id) return Boolean;
   pragma Inline (Initialization_Suppressed);
   --  Returns True if initialization should be suppressed for the given type
   --  or subtype. This is true if Suppress_Initialization is set either for
   --  the subtype itself, or for the corresponding base type.

   function Is_Body (N : Node_Id) return Boolean;
   --  Determine whether an arbitrary node denotes a body

   function Is_By_Copy_Type (Ent : Entity_Id) return Boolean;
   --  Ent is any entity. Returns True if Ent is a type entity where the type
   --  is required to be passed by copy, as defined in (RM 6.2(3)).

   function Is_By_Reference_Type (Ent : Entity_Id) return Boolean;
   --  Ent is any entity. Returns True if Ent is a type entity where the type
   --  is required to be passed by reference, as defined in (RM 6.2(4-9)).

   function Is_Derived_Type (Ent : Entity_Id) return Boolean;
   --  Determines if the given entity Ent is a derived type. Result is always
   --  false if argument is not a type.

   function Is_Generic_Formal (E : Entity_Id) return Boolean;
   --  Determine whether E is a generic formal parameter. In particular this is
   --  used to set the visibility of generic formals of a generic package
   --  declared with a box or with partial parameterization.

   function Is_Indefinite_Subtype (Ent : Entity_Id) return Boolean;
   --  Ent is any entity. Determines if given entity is an unconstrained array
   --  type or subtype, a discriminated record type or subtype with no initial
   --  discriminant values or a class wide type or subtype and returns True if
   --  so. False for other type entities, or any entities that are not types.

   function Is_Immutably_Limited_Type (Ent : Entity_Id) return Boolean;
   --  Implements definition in Ada 2012 RM-7.5 (8.1/3). This differs from the
   --  following predicate in that an untagged record with immutably limited
   --  components is NOT by itself immutably limited. This matters, e.g. when
   --  checking the legality of an access to the current instance.

   function Is_Limited_View (Ent : Entity_Id) return Boolean;
   --  Ent is any entity. True for a type that is "inherently" limited (i.e.
   --  cannot become nonlimited). From the Ada 2005 RM-7.5(8.1/2), "a type with
   --  a part that is of a task, protected, or explicitly limited record type".
   --  These are the types that are defined as return-by-reference types in Ada
   --  95 (see RM95-6.5(11-16)). In Ada 2005, these are the types that require
   --  build-in-place for function calls. Note that build-in-place is allowed
   --  for other types, too. This is also used for identifying pure procedures
   --  whose calls should not be eliminated (RM 10.2.1(18/2)).

   function Is_Limited_Type (Ent : Entity_Id) return Boolean;
   --  Ent is any entity. Returns true if Ent is a limited type (limited
   --  private type, limited interface type, task type, protected type,
   --  composite containing a limited component, or a subtype of any of
   --  these types). This older routine overlaps with the previous one, this
   --  should be cleaned up???

   function Nearest_Ancestor (Typ : Entity_Id) return Entity_Id;
   --  Given a subtype Typ, this function finds out the nearest ancestor from
   --  which constraints and predicates are inherited. There is no simple link
   --  for doing this, consider:
   --
   --     subtype R is Integer range 1 .. 10;
   --     type T is new R;
   --
   --  In this case the nearest ancestor is R, but the Etype of T'Base will
   --  point to R'Base, so we have to go rummaging in the declarations to get
   --  this information. It is used for making sure we freeze this before we
   --  freeze Typ, and also for retrieving inherited predicate information.
   --  For the case of base types or first subtypes, there is no useful entity
   --  to return, so Empty is returned.
   --
   --  Note: this is similar to Ancestor_Subtype except that it also deals
   --  with the case of derived types.

   function Nearest_Dynamic_Scope (Ent : Entity_Id) return Entity_Id;
   --  This is similar to Enclosing_Dynamic_Scope except that if Ent is itself
   --  a dynamic scope, then it is returned. Otherwise the result is the same
   --  as that returned by Enclosing_Dynamic_Scope.

   function Next_Tag_Component (Tag : Entity_Id) return Entity_Id;
   --  Tag must be an entity representing a _Tag field of a tagged record.
   --  The result returned is the next _Tag field in this record, or Empty
   --  if this is the last such field.

   function Number_Discriminants (Typ : Entity_Id) return Pos;
   --  Typ is a type with discriminants, yields number of discriminants in type

   function Object_Type_Has_Constrained_Partial_View
     (Typ  : Entity_Id;
      Scop : Entity_Id) return Boolean;
   --  Return True if type of object has attribute Has_Constrained_Partial_View
   --  set to True; in addition, within a generic body, return True if subtype
   --  of the object is a descendant of an untagged generic formal private or
   --  derived type, and the subtype is not an unconstrained array subtype
   --  (RM 3.3(23.10/3)).

   function Ultimate_Alias (Prim : Entity_Id) return Entity_Id;
   pragma Inline (Ultimate_Alias);
   --  Return the last entity in the chain of aliased entities of Prim. If Prim
   --  has no alias return Prim.

   function Unit_Declaration_Node (Unit_Id : Entity_Id) return Node_Id;
   --  Unit_Id is the simple name of a program unit, this function returns the
   --  corresponding xxx_Declaration node for the entity. Also applies to the
   --  body entities for subprograms, tasks and protected units, in which case
   --  it returns the subprogram, task or protected body node for it. The unit
   --  may be a child unit with any number of ancestors.

   function Package_Specification (Pack_Id : Entity_Id) return Node_Id;
   --  Given an entity for a package or generic package, return corresponding
   --  package specification. Simplifies handling of child units, and better
   --  than the old idiom: Specification (Unit_Declaration_Node (Pack_Id)).
end Sem_Aux;
