------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . E X C E P T I O N S . M A C H I N E           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2013, Free Software Foundation, Inc.            --
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

--  This is the version using the GCC EH mechanism

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

package System.Exceptions.Machine is
   pragma Preelaborate;

   ------------------------------------------------
   -- Entities to interface with the GCC runtime --
   ------------------------------------------------

   --  These come from "C++ ABI for Itanium: Exception handling", which is
   --  the reference for GCC.

   --  Return codes from the GCC runtime functions used to propagate
   --  an exception.

   type Unwind_Reason_Code is
     (URC_NO_REASON,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_PHASE2_ERROR,
      URC_PHASE1_ERROR,
      URC_NORMAL_STOP,
      URC_END_OF_STACK,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND);

   pragma Unreferenced
     (URC_NO_REASON,
      URC_FOREIGN_EXCEPTION_CAUGHT,
      URC_PHASE2_ERROR,
      URC_PHASE1_ERROR,
      URC_NORMAL_STOP,
      URC_END_OF_STACK,
      URC_HANDLER_FOUND,
      URC_INSTALL_CONTEXT,
      URC_CONTINUE_UNWIND);

   pragma Convention (C, Unwind_Reason_Code);

   --  Phase identifiers

   type Unwind_Action is new Integer;
   pragma Convention (C, Unwind_Action);

   UA_SEARCH_PHASE  : constant Unwind_Action := 1;
   UA_CLEANUP_PHASE : constant Unwind_Action := 2;
   UA_HANDLER_FRAME : constant Unwind_Action := 4;
   UA_FORCE_UNWIND  : constant Unwind_Action := 8;
   UA_END_OF_STACK  : constant Unwind_Action := 16;  --  GCC extension

   pragma Unreferenced
     (UA_SEARCH_PHASE,
      UA_CLEANUP_PHASE,
      UA_HANDLER_FRAME,
      UA_FORCE_UNWIND,
      UA_END_OF_STACK);

   --  Mandatory common header for any exception object handled by the
   --  GCC unwinding runtime.

   type Exception_Class is mod 2 ** 64;

   GNAT_Exception_Class : constant Exception_Class := 16#474e552d41646100#;
   --  "GNU-Ada\0"

   type Unwind_Word is mod 2 ** System.Word_Size;
   for Unwind_Word'Size use System.Word_Size;
   --  Map the corresponding C type used in Unwind_Exception below

   type Unwind_Exception is record
      Class    : Exception_Class;
      Cleanup  : System.Address;
      Private1 : Unwind_Word;
      Private2 : Unwind_Word;

      --  Usual exception structure has only two private fields, but the SEH
      --  one has six. To avoid making this file more complex, we use six
      --  fields on all platforms, wasting a few bytes on some.

      Private3 : Unwind_Word;
      Private4 : Unwind_Word;
      Private5 : Unwind_Word;
      Private6 : Unwind_Word;
   end record;
   pragma Convention (C, Unwind_Exception);
   --  Map the GCC struct used for exception handling

   for Unwind_Exception'Alignment use Standard'Maximum_Alignment;
   --  The C++ ABI mandates the common exception header to be at least
   --  doubleword aligned, and the libGCC implementation actually makes it
   --  maximally aligned (see unwind.h). See additional comments on the
   --  alignment below.

   --  There is a subtle issue with the common header alignment, since the C
   --  version is aligned on BIGGEST_ALIGNMENT, the Ada version is aligned on
   --  Standard'Maximum_Alignment, and those two values don't quite represent
   --  the same concepts and so may be decoupled someday. One typical reason
   --  is that BIGGEST_ALIGNMENT may be larger than what the underlying system
   --  allocator guarantees, and there are extra costs involved in allocating
   --  objects aligned to such factors.

   --  To deal with the potential alignment differences between the C and Ada
   --  representations, the Ada part of the whole structure is only accessed
   --  by the personality routine through accessors. Ada specific fields are
   --  thus always accessed through consistent layout, and we expect the
   --  actual alignment to always be large enough to avoid traps from the C
   --  accesses to the common header. Besides, accessors alleviate the need
   --  for a C struct whole counterpart, both painful and error-prone to
   --  maintain anyway.

   type GCC_Exception_Access is access all Unwind_Exception;
   --  Pointer to a GCC exception. Do not use convention C as on VMS this
   --  would imply the use of 32-bits pointers.

   procedure Unwind_DeleteException (Excp : not null GCC_Exception_Access);
   pragma Import (C, Unwind_DeleteException, "_Unwind_DeleteException");
   --  Procedure to free any GCC exception

   --------------------------------------------------------------
   -- GNAT Specific Entities To Deal With The GCC EH Circuitry --
   --------------------------------------------------------------

   --  A GNAT exception object to be dealt with by the personality routine
   --  called by the GCC unwinding runtime.

   type GNAT_GCC_Exception is record
      Header : Unwind_Exception;
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

   function New_Occurrence return GNAT_GCC_Exception_Access is
      (new GNAT_GCC_Exception'
        (Header     => (Class   => GNAT_Exception_Class,
                        Cleanup => Null_Address,
                        others  => 0),
         Occurrence => <>));
   --  Allocate and initialize a machine occurrence

end System.Exceptions.Machine;
