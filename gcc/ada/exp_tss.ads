------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ T S S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

   --  A TSS is identified by its Chars name, i.e. for a given TSS type, the
   --  same name is used for all types, e.g. the initialization routine has
   --  the name _init for all types.

   --  The TSS's for a given type are stored in an element list associated with
   --  the type, and referenced from the TSS_Elist field of the N_Freeze_Entity
   --  node associated with the type (all types that need TSS's always need to
   --  be explicitly frozen, so the N_Freeze_Entity node always exists).

   function TSS (Typ : Entity_Id; Nam : Name_Id) return Entity_Id;
   --  Finds the TSS with the given name associated with the given type. If
   --  no such TSS exists, then Empty is returned.

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
   --  _init TSR, but initialization is required. The initialization procedure
   --  used is the one fot the corresponding record type (see Base_Init_Proc).

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
   --  is used to test for the presence of an Init_Proc in cases where
   --  a null init proc is considered equivalent to no Init_Proc.

end Exp_Tss;
