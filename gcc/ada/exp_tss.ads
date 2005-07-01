------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ T S S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Type Support Subprogram (TSS) handling

with Types; use Types;

package Exp_Tss is

   --  A type support subprogram (TSS) is an internally generated function or
   --  procedure that is associated with a particular type. Examples are the
   --  implicit initialization procedure, and subprograms for the Input and
   --  Output attributes.

   --  A given TSS is either generated once at the point of the declaration of
   --  the type, or it is generated as needed in clients, but only one copy is
   --  required in any one generated object file. The choice between these two
   --  possibilities is made on a TSS-by-TSS basis depending on the estimation
   --  of how likely the TSS is to be used. Initialization procedures fall in
   --  the first category, for example, since it is likely that any declared
   --  type will be used in a context requiring initialization, but the stream
   --  attributes use the second approach, since it is more likely that they
   --  will not be used at all, or will only be used in one client in any case.

   -------------------------
   -- Current Limitations --
   -------------------------

   --  In the current version of this package, only the case of generating a
   --  TSS at the point of declaration of the type is accomodated. A clear
   --  improvement would be to follow through with the full implementation
   --  as described above, and also accomodate the requirement of generating
   --  only one copy in a given object file.

   --  For now, we deal with the local case by generating duplicate versions
   --  of the TSS routine, which is clearly rather inefficient in space usage.
   --  This is done by using Make_TSS_Name_Local to generate unique names
   --  for the different instances of TSS routines in a given scope.

   ----------------
   -- TSS Naming --
   ----------------

   --  A TSS is identified by its Chars name. The name has the form typXY or
   --  typ_<serial>XY, where typ is the type name, and XY are two characters
   --  that identify the particular TSS routine. A unique serial number is
   --  included for the case where several local instances of the same TSS
   --  must be generated (see discussion under Make_TSS_Name_Local).

   --  The following codes are used to denote TSSs:

   --  Note: When making additions to this list, update the list in snames.adb

   type TSS_Name_Type is new String (1 .. 2);
   subtype TNT is TSS_Name_Type;

   TSS_Deep_Adjust        : constant TNT := "DA";  -- Deep Adjust
   TSS_Deep_Finalize      : constant TNT := "DF";  -- Deep Finalize
   TSS_Deep_Initialize    : constant TNT := "DI";  -- Deep Initialize
   TSS_Composite_Equality : constant TNT := "EQ";  -- Composite Equality
   TSS_From_Any           : constant TNT := "FA";  -- PolyORB/DSA From_Any
   TSS_Init_Proc          : constant TNT := "IP";  -- Initialization Procedure
   TSS_RAS_Access         : constant TNT := "RA";  -- RAS type access
   TSS_RAS_Dereference    : constant TNT := "RD";  -- RAS type deference
   TSS_Rep_To_Pos         : constant TNT := "RP";  -- Rep to Pos conversion
   TSS_Slice_Assign       : constant TNT := "SA";  -- Slice assignment
   TSS_Stream_Input       : constant TNT := "SI";  -- Stream Input attribute
   TSS_Stream_Output      : constant TNT := "SO";  -- Stream Output attribute
   TSS_Stream_Read        : constant TNT := "SR";  -- Stream Read attribute
   TSS_Stream_Write       : constant TNT := "SW";  -- Stream Write attribute
   TSS_To_Any             : constant TNT := "TA";  -- PolyORB/DSA To_Any
   TSS_TypeCode           : constant TNT := "TC";  -- PolyORB/DSA TypeCode

   --  The array below contains all valid TSS names

   TSS_Names : constant array (Natural range <>) of TSS_Name_Type :=
     (TSS_Deep_Adjust,
      TSS_Deep_Finalize,
      TSS_Deep_Initialize,
      TSS_Composite_Equality,
      TSS_From_Any,
      TSS_Init_Proc,
      TSS_RAS_Access,
      TSS_RAS_Dereference,
      TSS_Rep_To_Pos,
      TSS_Slice_Assign,
      TSS_Stream_Input,
      TSS_Stream_Output,
      TSS_Stream_Read,
      TSS_Stream_Write,
      TSS_To_Any,
      TSS_TypeCode);

   TSS_Null : constant TNT := "  ";
   --  Dummy entry used to indicated that this is not really a TSS

   function Get_TSS_Name (E : Entity_Id) return TSS_Name_Type;
   --  Given an entity, if it is a TSS, then return the corresponding TSS
   --  name type, otherwise return TSS_Null.

   function Make_TSS_Name
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Name_Id;
   --  Construct the name as described above for the given TSS routine
   --  identified by Nam for the type identified by Typ.

   function Make_TSS_Name_Local
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Name_Id;
   --  Similar to the above call, but a string of the form _nnn is inserted
   --  before the TSS code suffix, where nnn is a unique serial number. This
   --  is used when multiple instances of the same TSS routine may be
   --  generated in the same scope (see also discussion above of current
   --  limitations).

   function Make_Init_Proc_Name (Typ : Entity_Id) return Name_Id;
   --  Version for init procs, same as Make_TSS_Name (Typ, TSS_Init_Proc)

   function Is_TSS (E : Entity_Id; Nam : TSS_Name_Type) return Boolean;
   --  Determines if given entity (E) is the name of a TSS identified by Nam

   function Is_TSS (N : Name_Id; Nam : TSS_Name_Type) return Boolean;
   --  Same test applied directly to a Name_Id value

   function Is_Init_Proc (E : Entity_Id) return Boolean;
   --  Version for init procs, same as Is_TSS (E, TSS_Init_Proc);

   -----------------------------------------
   -- TSS Data structures and Subprograms --
   -----------------------------------------

   --  The TSS's for a given type are stored in an element list associated with
   --  the type, and referenced from the TSS_Elist field of the N_Freeze_Entity
   --  node associated with the type (all types that need TSS's always need to
   --  be explicitly frozen, so the N_Freeze_Entity node always exists).

   function TSS (Typ : Entity_Id; Nam : TSS_Name_Type) return Entity_Id;
   --  Finds the TSS with the given name associated with the given type
   --  If no such TSS exists, then Empty is returned;

   function TSS (Typ : Entity_Id; Nam : Name_Id) return Entity_Id;
   --  Finds the TSS with the given name associated with the given type. If
   --  no such TSS exists, then Empty is returned.

   function Same_TSS (E1, E2 : Entity_Id) return Boolean;
   --  Returns True if E1 and E2 are the same kind of TSS, even if the names
   --  are different (i.e. if the names of E1 and E2 end with two upper case
   --  letters that are the same).

   procedure Set_TSS (Typ : Entity_Id; TSS : Entity_Id);
   --  This procedure is used to install a newly created TSS. The second
   --  argument is the entity for such a new TSS. This entity is placed in
   --  the TSS list for the type given as the first argument, replacing an
   --  old entry of the same name if one was present. The tree for the body
   --  of this TSS, which is not analyzed yet, is placed in the actions field
   --  of the freeze node for the type. All such bodies are inserted into the
   --  main tree and analyzed at the point at which the freeze node itself is
   --  is expanded.

   procedure Copy_TSS (TSS : Entity_Id; Typ : Entity_Id);
   --  Given an existing TSS for another type (which is already installed,
   --  analyzed and expanded), install it as the corresponding TSS for Typ.
   --  Note that this just copies a reference, not the tree. This can also
   --  be used to initially install a TSS in the case where the subprogram
   --  for the TSS has already been created and its declaration processed.

   function Init_Proc (Typ : Entity_Id) return Entity_Id;
   pragma Inline (Init_Proc);
   --  Obtains the _init TSS entry for the given type. This function call is
   --  equivalent to TSS (Typ, Name_uInit). The _init TSS is the procedure
   --  used to initialize otherwise uninitialized instances of a type. If
   --  there is no _init TSS, then the type requires no initialization. Note
   --  that subtypes and implicit types never have an _init TSS since subtype
   --  objects are always initialized using the initialization procedure for
   --  the corresponding base type (see Base_Init_Proc function). A special
   --  case arises for concurrent types. Such types do not themselves have an
   --  init proc TSS, but initialization is required. The init proc used is
   --  the one fot the corresponding record type (see Base_Init_Proc).

   function Base_Init_Proc (Typ : Entity_Id) return Entity_Id;
   --  Obtains the _Init TSS entry from the base type of the entity, and also
   --  deals with going indirect through the Corresponding_Record_Type field
   --  for concurrent objects (which are initialized with the initialization
   --  routine for the corresponding record type). Returns Empty if there is
   --  no _Init TSS entry for the base type.

   procedure Set_Init_Proc (Typ : Entity_Id; Init : Entity_Id);
   pragma Inline (Set_Init_Proc);
   --  The second argument is the _init TSS to be established for the type
   --  given as the first argument. Equivalent to Set_TSS (Typ, Init).

   function Has_Non_Null_Base_Init_Proc (Typ : Entity_Id) return Boolean;
   --  Returns true if the given type has a defined Base_Init_Proc and
   --  this init proc is not a null init proc (null init procs occur as
   --  a result of the processing for Initialize_Scalars. This function
   --  is used to test for the presence of an init proc in cases where
   --  a null init proc is considered equivalent to no init proc.

   function Find_Inherited_TSS
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id;
   --  Returns the TSS of name Nam of Typ, or of its closest ancestor defining
   --  such a TSS. Empty is returned is neither Typ nor any of its ancestors
   --  have such a TSS.

end Exp_Tss;
