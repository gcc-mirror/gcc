------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  G N A T . C P P _ E X C E P T I O N S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (C) 2013-2025, AdaCore                  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;
with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;
with System.Standard_Library; use System.Standard_Library;

package body GNAT.CPP_Exceptions is

   --  Note: all functions prefixed by __cxa are part of the c++ ABI for
   --  exception handling. As they are provided by the c++ library, there
   --  must be no dependencies on it in the compiled code of this unit, but
   --  there can be dependencies in instances. This is required to be able
   --  to build the shared library without the c++ library.

   function To_Exception_Data_Ptr is new
     Ada.Unchecked_Conversion
       (Exception_Id, Exception_Data_Ptr);
   --  Convert an Exception_Id to its non-private type. This is used to get
   --  the RTTI of a C++ exception

   function Get_Exception_Machine_Occurrence
     (X : Exception_Occurrence) return System.Address;
   pragma Import (Ada, Get_Exception_Machine_Occurrence,
                    "__gnat_get_exception_machine_occurrence");
   --  Imported function (from Ada.Exceptions) that returns the machine
   --  occurrence from an exception occurrence.

   -------------------------
   -- Raise_Cpp_Exception --
   -------------------------

   procedure Raise_Cpp_Exception (Id : Exception_Id; Value : T)
   is
      Id_Data : constant Exception_Data_Ptr := To_Exception_Data_Ptr (Id);
      --  Get a non-private view on the exception

      type T_Acc is access all T;
      pragma Convention (C, T_Acc);
      --  Access type to the object compatible with C

      Occ : T_Acc;
      --  The occurrence to propagate

      function cxa_allocate_exception (Size : size_t) return T_Acc;
      pragma Import (C, cxa_allocate_exception, "__cxa_allocate_exception");
      --  The C++ function to allocate an occurrence

      procedure cxa_throw (Obj : T_Acc; Tinfo : System.Address;
                                        Dest :  System.Address);
      pragma Import (C, cxa_throw, "__cxa_throw");
      pragma No_Return (cxa_throw);
      --  The C++ function to raise an exception
   begin
      --  Check the exception was imported from C++

      if Id_Data.Lang not in 'B' | 'C' then
         raise Constraint_Error;
      end if;

      --  Allocate the C++ occurrence

      Occ := cxa_allocate_exception (T'Size / System.Storage_Unit);

      --  Set the object

      Occ.all := Value;

      --  Throw the exception

      cxa_throw (Occ, Id_Data.Foreign_Data, System.Null_Address);
   end Raise_Cpp_Exception;

   ------------------------
   -- Get_Object_Address --
   ------------------------

   function Get_Object_Address
     (X : Exception_Occurrence) return System.Address
   is
      Exception_Addr : constant Address :=
        Get_Exception_Machine_Occurrence (X);
      --  Machine occurrence of X

      Object_Addr : Address;
      --  Address of the raised object, after calling Convert

      Id : constant Exception_Id := Exception_Identity (X);

      Id_Data : constant Exception_Data_Ptr := To_Exception_Data_Ptr (Id);
      --  Get a non-private view on the exception

      Success : Integer;

      procedure Convert (Success     : out Integer;
                         Object_Addr : out Address;
                         Catch_Type  : Address;
                         Convention  : Character;
                         Unw_Except  : Address);
      pragma Import (C, Convert, "__gnat_obtain_caught_object");

   begin
      --  Check the machine occurrence exists

      if Exception_Addr = Null_Address then
         raise Constraint_Error;
      end if;

      if Id_Data.Lang not in 'A' .. 'C' then
         raise Constraint_Error;
      end if;

      Convert (Success, Object_Addr,
               Id_Data.Foreign_Data, Id_Data.Lang,
               Exception_Addr);
      if Success = 0 then
         raise Constraint_Error;
      end if;

      return Object_Addr;
   end Get_Object_Address;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (X : Exception_Occurrence) return T
   is
      Object_Addr : constant System.Address := Get_Object_Address (X);
      --  Address of the raised object

      --  Import the object from the occurrence
      Result : constant T;
      pragma Import (Ada, Result);
      for Result'Address use Object_Addr;
   begin
      return Result;
   end Get_Object;

   --------------------------
   -- Get_Access_To_Object --
   --------------------------

   function Get_Access_To_Object (X : Exception_Occurrence)
                                 return access T
   is
      Object_Addr : constant System.Address := Get_Object_Address (X);
      --  Address of the raised object

      type T_Acc is access T;

      function To_T_Acc is
         new Ada.Unchecked_Conversion (System.Address, T_Acc);

      --  Import the object from the occurrence
      Result : constant T_Acc := To_T_Acc (Object_Addr);
   begin
      return Result;
   end Get_Access_To_Object;

   ---------------------------------
   -- Get_Access_To_Tagged_Object --
   ---------------------------------

   function Get_Access_To_Tagged_Object (X : Exception_Occurrence)
                                        return access T'Class
   is
      Object_Addr : constant System.Address := Get_Object_Address (X);
      --  Address of the raised object

      type T_Acc is access T'Class;

      function To_T_Acc is
         new Ada.Unchecked_Conversion (System.Address, T_Acc);

      --  Import the object from the occurrence
      Result : constant T_Acc := To_T_Acc (Object_Addr);
   begin
      return Result;
   end Get_Access_To_Tagged_Object;

   -------------------
   -- Get_Type_Info --
   -------------------

   function Get_Type_Info (Id : Exception_Id) return Type_Info_Ptr is
      Id_Data : constant Exception_Data_Ptr := To_Exception_Data_Ptr (Id);
      --  Get a non-private view on the exception

      Foreign_Exception : aliased Exception_Data;
      pragma Import
        (Ada, Foreign_Exception, "system__exceptions__foreign_exception");
   begin
      if Id_Data.Lang not in 'B' | 'C' then
         if Id_Data.Lang = 'A'
           and then
           Id_Data = Foreign_Exception'Unchecked_Access
         then
            return No_Type_Info;
         end if;
         raise Constraint_Error;
      end if;

      return To_Type_Info_Ptr (Id_Data.Foreign_Data);
   end Get_Type_Info;

   -------------------
   -- Get_Type_Info --
   -------------------

   function Get_Type_Info (X : Exception_Occurrence) return Type_Info_Ptr is
      Exception_Addr : constant Address :=
        Get_Exception_Machine_Occurrence (X);
      --  Machine occurrence of X

      function Get_MO_RTTI (MO : Address) return Type_Info_Ptr;
      pragma Import (Cpp, Get_MO_RTTI, "__gnat_get_cxx_exception_type_info");

      TI : Type_Info_Ptr;
   begin
      if Exception_Addr = Null_Address then
         raise Constraint_Error;
      end if;

      if To_Exception_Data_Ptr (Exception_Identity (X)).Lang
        not in 'A' .. 'C'
      then
         raise Constraint_Error;
      end if;

      TI := Get_MO_RTTI (Exception_Addr);

      if TI = No_Type_Info then
         raise Constraint_Error;
      end if;

      return TI;

   end Get_Type_Info;

end GNAT.CPP_Exceptions;
