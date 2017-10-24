------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               L A Y O U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2017, Free Software Foundation, Inc.         --
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

--  This package does front-end layout of types and objects. The result is
--  to annotate the tree with information on size and alignment of types
--  and objects. How much layout is performed depends on the setting of the
--  target dependent parameter Backend_Layout.

with Types; use Types;

package Layout is

   --  The following procedures are called from Freeze, so all entities
   --  for types and objects that get frozen (which should be all such
   --  entities which are seen by the back end) will get layed out by one
   --  of these two procedures.

   procedure Layout_Type (E : Entity_Id);
   --  This procedure may set or adjust the fields Esize, RM_Size and
   --  Alignment in the non-generic type or subtype entity E. If the
   --  Backend_Layout switch is False, then it is guaranteed that all
   --  three fields will be properly set on return. Regardless of the
   --  Backend_Layout value, it is guaranteed that all discrete types
   --  will have both Esize and RM_Size fields set on return (since
   --  these are static values). Note that Layout_Type is not called
   --  for generic types, since these play no part in code generation,
   --  and hence representation aspects are irrelevant.

   procedure Layout_Object (E : Entity_Id);
   --  E is either a variable (E_Variable), a constant (E_Constant),
   --  a loop parameter (E_Loop_Parameter), or a formal parameter of
   --  a non-generic subprogram (E_In_Parameter, E_In_Out_Parameter,
   --  or E_Out_Parameter). This procedure may set or adjust the
   --  Esize and Alignment fields of E. If Backend_Layout is False,
   --  then it is guaranteed that both fields will be properly set
   --  on return. If the Esize is still unknown in the latter case,
   --  it means that the object must be allocated dynamically, since
   --  its length is not known at compile time.

   --  The following are utility routines, called from various places

   procedure Adjust_Esize_Alignment (E : Entity_Id);
   --  E is the entity for a type or object. This procedure checks that the
   --  size and alignment are compatible, and if not either gives an error
   --  message if they cannot be adjusted or else adjusts them appropriately.

   procedure Set_Discrete_RM_Size (Def_Id : Entity_Id);
   --  Set proper RM_Size for discrete size, this is normally the minimum
   --  number of bits to accommodate the range given, except in the case
   --  where the subtype statically matches the first subtype, in which
   --  case the size must be copied from the first subtype. For generic
   --  types, the RM_Size is simply set to zero. This routine also sets
   --  the Is_Constrained flag in Def_Id.

   procedure Set_Elem_Alignment (E : Entity_Id; Align : Nat := 0);
   --  The front end always sets alignments for elementary types by calling
   --  this procedure. Note that we have to do this for discrete types (since
   --  the Alignment attribute is static), so we might as well do it for all
   --  elementary types, as the processing is the same. If Align is nonzero,
   --  it is an external alignment setting that we must respect.

end Layout;
