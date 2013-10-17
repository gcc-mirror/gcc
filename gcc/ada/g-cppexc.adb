------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  G N A T . C P P _ E X C E P T I O N S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with System;
with System.Storage_Elements;
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

      if Id_Data.Lang /= 'C' then
         raise Constraint_Error;
      end if;

      --  Allocate the C++ occurrence

      Occ := cxa_allocate_exception (T'Size / System.Storage_Unit);

      --  Set the object

      Occ.all := Value;

      --  Throw the exception

      cxa_throw (Occ, Id_Data.Foreign_Data, System.Null_Address);
   end Raise_Cpp_Exception;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (X : Exception_Occurrence) return T
   is
      use System;
      use System.Storage_Elements;

      Unwind_Exception_Size : Natural;
      pragma Import (C, Unwind_Exception_Size, "__gnat_unwind_exception_size");
      --  Size in bytes of _Unwind_Exception

      Exception_Addr : constant Address :=
        Get_Exception_Machine_Occurrence (X);
      --  Machine occurrence of X

   begin
      --  Check the machine occurrence exists

      if Exception_Addr = Null_Address then
         raise Constraint_Error;
      end if;

      declare
         --  Import the object from the occurrence
         Result : T;
         pragma Import (Ada, Result);
         for Result'Address use
            Exception_Addr + Storage_Offset (Unwind_Exception_Size);
      begin
         --  And return it
         return Result;
      end;
   end Get_Object;
end GNAT.CPP_Exceptions;
