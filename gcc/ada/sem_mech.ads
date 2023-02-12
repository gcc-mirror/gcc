------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ M E C H                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2023, Free Software Foundation, Inc.         --
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

--  This package contains the routine used to establish calling mechanisms
--  The reason we separate this off into its own package is that it is
--  entirely possible that it may need some target specific specialization.

with Types; use Types;

package Sem_Mech is

   -------------------------------------------------
   -- Definitions for Parameter Mechanism Control --
   -------------------------------------------------

   --  For parameters passed to subprograms, and for function return values,
   --  a passing mechanism is defined. The entity attribute Mechanism returns
   --  an indication of the mechanism, and Set_Mechanism can be used to set
   --  the mechanism. At the program level, there are three ways to explicitly
   --  set the mechanism:

   --    An Import_xxx or Export_xxx pragma (where xxx is Function, Procedure,
   --    or Valued_Procedure) can explicitly set the mechanism for either a
   --    parameter or a function return value. A mechanism explicitly set by
   --    such a pragma overrides the effect of C_Pass_By_Copy described below.

   --    If convention C_Pass_By_Copy is set for a record, and the record type
   --    is used as the formal type of a subprogram with a foreign convention,
   --    then the mechanism is set to By_Copy.

   --    If a pragma C_Pass_By_Copy applies, and a record type has Convention
   --    C, and the record type is used as the formal type of a subprogram
   --    with a foreign convention, then the mechanism is set to use By_Copy
   --    if the size of the record is sufficiently small (as determined by
   --    the value of the parameter to pragma C_Pass_By_Copy).

   --  The subtype Mechanism_Type (declared in Types) is used to describe
   --  the mechanism to be used. The following special values of this type
   --  specify the mechanism, as follows.

   Default_Mechanism : constant Mechanism_Type := 0;
   --  The default setting indicates that the backend will choose the proper
   --  default mechanism. This depends on the convention of the subprogram
   --  involved, and is generally target dependent. In the compiler, the
   --  backend chooses the mechanism in this case in accordance with any
   --  requirements imposed by the ABI. Note that Default is never used for
   --  record types on foreign convention subprograms, since By_Reference
   --  is forced for such types unless one of the above described approaches
   --  is used to explicitly force By_Copy.

   By_Copy : constant Mechanism_Type := -1;
   --  Passing by copy is forced. The exact meaning of By_Copy (e.g. whether
   --  at a low level the value is passed in registers, or the value is copied
   --  and a pointer is passed), is determined by the backend in accordance
   --  with requirements imposed by the ABI. Note that in the extended import
   --  and export pragma mechanisms, this is called Value, rather than Copy.

   By_Reference : constant Mechanism_Type := -2;
   --  Passing by reference is forced. This is always equivalent to passing
   --  a simple pointer in the case of subprograms with a foreign convention.
   --  For unconstrained arrays passed to foreign convention subprograms, the
   --  address of the first element of the array is passed. For convention
   --  Ada, the result is logically to pass a reference, but the precise
   --  mechanism (e.g. to pass bounds of unconstrained types and other needed
   --  special information) is determined by the backend in accordance with
   --  requirements imposed by the ABI as interpreted for Ada.

   pragma Assert (Mechanism_Type'First = -2);
   --  Check definition in types is right!

   --  All the above special values are non-positive. Positive values for
   --  Mechanism_Type values have a special meaning. They are used only in
   --  the case of records, as a result of the use of the C_Pass_By_Copy
   --  pragma, and the meaning is that if the size of the record is known
   --  at compile time and does not exceed the mechanism type value, then
   --  By_Copy passing is forced, otherwise By_Reference is forced.

   ----------------------
   -- Global Variables --
   ----------------------

   Default_C_Record_Mechanism : Mechanism_Type := By_Reference;
   --  This value is the default mechanism used for C convention records
   --  in foreign-convention subprograms if no mechanism is otherwise
   --  specified. This value is modified appropriately by the occurrence
   --  of a C_Pass_By_Copy configuration pragma.

   -----------------
   -- Subprograms --
   -----------------

   procedure Set_Mechanisms (E : Entity_Id);
   --  E is a subprogram or subprogram type that has been frozen, so the
   --  convention of the subprogram and all its formal types and result
   --  type in the case of a function are established. The function of
   --  this call is to set mechanism values for formals and for the
   --  function return if they have not already been explicitly set by
   --  a use of an extended Import or Export pragma. The idea is to set
   --  mechanism values wherever the semantics is dictated by either
   --  requirements or implementation advice in the RM, and to leave
   --  the mechanism set to Default if there is no requirement, so that
   --  the back-end is free to choose the most efficient method.

   procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id);
   --  Mech is a parameter passing mechanism (see Import_Function syntax
   --  for MECHANISM_NAME). This routine checks that the mechanism argument
   --  has the right form, and if not issues an error message. If the
   --  argument has the right form then the Mechanism field of Ent is
   --  set appropriately. It also performs some error checks. Note that
   --  the mechanism name has not been analyzed (and cannot indeed be
   --  analyzed, since it is semantic nonsense), so we get it in the
   --  exact form created by the parser.

   procedure Set_Mechanism_With_Checks
     (Ent  : Entity_Id;
      Mech : Mechanism_Type;
      Enod : Node_Id);
   --  Sets the mechanism of Ent to the given Mech value, after first checking
   --  that the request makes sense. If it does not make sense, a warning is
   --  posted on node Enod, and the Mechanism of Ent is unchanged.

end Sem_Mech;
