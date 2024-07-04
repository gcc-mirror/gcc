------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . E X C E P T I O N S . M A C H I N E           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2013-2024, Free Software Foundation, Inc.          --
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

--  Declaration of the machine exception and some associated facilities. The
--  machine exception is the object that is propagated by low level routines
--  and that contains the Ada exception occurrence.

--  This is the version using the ARM EHABI mechanism

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

package System.Exceptions.Machine is
   pragma Preelaborate;

   ------------------------------------------------
   -- Entities to interface with the GCC runtime --
   ------------------------------------------------

   --  Return codes from GCC runtime functions used to propagate an exception

   type Unwind_Reason_Code is
     (URC_OK,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_Unused2,
      URC_Unused3,
      URC_Unused4,
      URC_Unused5,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND,
      URC_FAILURE);
   pragma Convention (C, Unwind_Reason_Code);

   pragma Unreferenced
     (URC_OK,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_Unused2,
      URC_Unused3,
      URC_Unused4,
      URC_Unused5,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND,
      URC_FAILURE);

   --  ARM Unwinding State

   type uint32_t is mod 2**32;
   pragma Convention (C, uint32_t);

   type uint32_t_array is array (Natural range <>) of uint32_t;
   pragma Convention (C, uint32_t_array);

   type Unwind_State is new uint32_t;
   pragma Convention (C, Unwind_State);

   US_VIRTUAL_UNWIND_FRAME  : constant Unwind_State := 0;
   US_UNWIND_FRAME_STARTING : constant Unwind_State := 1;
   US_UNWIND_FRAME_RESUME   : constant Unwind_State := 2;

   pragma Unreferenced
     (US_VIRTUAL_UNWIND_FRAME,
      US_UNWIND_FRAME_STARTING,
      US_UNWIND_FRAME_RESUME);

   --  Mandatory common header for any exception object handled by the
   --  GCC unwinding runtime.

   type Exception_Class is array (0 .. 7) of Character;

   GNAT_Exception_Class : constant Exception_Class := "GNU-Ada" & ASCII.NUL;
   --  "GNU-Ada\0"

   type Unwinder_Cache_Type is record
      Reserved1 : uint32_t;
      Reserved2 : uint32_t;
      Reserved3 : uint32_t;
      Reserved4 : uint32_t;
      Reserved5 : uint32_t;
   end record;

   type Barrier_Cache_Type is record
      Sp         : uint32_t;
      Bitpattern : uint32_t_array (0 .. 4);
   end record;

   type Cleanup_Cache_Type is record
     Bitpattern : uint32_t_array (0 .. 3);
   end record;

   type Pr_Cache_Type is record
      Fnstart    : uint32_t;
      Ehtp       : System.Address;
      Additional : uint32_t;
      Reserved1  : uint32_t;
   end record;

   type Unwind_Control_Block is record
      Class   : Exception_Class;
      Cleanup : System.Address;

      --  Caches
      Unwinder_Cache : Unwinder_Cache_Type;
      Barrier_Cache  : Barrier_Cache_Type;
      Cleanup_Cache  : Cleanup_Cache_Type;
      Pr_Cache       : Pr_Cache_Type;
   end record;
   pragma Convention (C, Unwind_Control_Block);
   for Unwind_Control_Block'Alignment use 8;
   --  Map the GCC struct used for exception handling

   type Unwind_Control_Block_Access is access all Unwind_Control_Block;
   subtype GCC_Exception_Access is Unwind_Control_Block_Access;
   --  Pointer to a UCB

   procedure Unwind_DeleteException
     (Ucbp : not null Unwind_Control_Block_Access);
   pragma Import (C, Unwind_DeleteException, "_Unwind_DeleteException");
   --  Procedure to free any GCC exception

   --------------------------------------------------------------
   -- GNAT Specific Entities To Deal With The GCC EH Circuitry --
   --------------------------------------------------------------

   --  A GNAT exception object to be dealt with by the personality routine
   --  called by the GCC unwinding runtime.

   type GNAT_GCC_Exception is record
      Header : Unwind_Control_Block;
      --  ABI Exception header first

      Occurrence : aliased Ada.Exceptions.Exception_Occurrence;
      --  The Ada occurrence
   end record;

   pragma Convention (C, GNAT_GCC_Exception);

   type GNAT_GCC_Exception_Access is access all GNAT_GCC_Exception;

   function To_GCC_Exception is new
     Ada.Unchecked_Conversion (System.Address, GCC_Exception_Access);

   function To_GNAT_GCC_Exception is new
     Ada.Unchecked_Conversion
     (GCC_Exception_Access, GNAT_GCC_Exception_Access);

   function New_Occurrence return GNAT_GCC_Exception_Access;
   --  Allocate and initialize a machine occurrence

end System.Exceptions.Machine;
