------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . E X C E P T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  This package contains definitions used for zero cost exception handling.
--  See unit Ada.Exceptions for further details. Note that the reason that
--  we separate out these definitions is to avoid problems with recursion
--  in rtsfind. They must be in a unit which does not require any exception
--  table generation of any kind.

with Ada.Exceptions;

with System;
with System.Standard_Library;

with Unchecked_Conversion;

package System.Exceptions is

   package SSL renames System.Standard_Library;
   package AEX renames Ada.Exceptions;

   --  The following section defines data structures used for zero cost
   --  exception handling if System.Parameters.Zero_Cost_Exceptions is
   --  set true (i.e. zero cost exceptions are implemented on this target).

   --  The approach is to build tables that describe the PC ranges that
   --  are covered by various exception frames. When an exception occurs,
   --  these tables are searched to determine the address of the applicable
   --  handler for the current exception.

   subtype Handler_Loc is System.Address;
   --  Code location representing entry address of a handler. Values of
   --  this type are created using the N_Handler_Loc node, and then
   --  passed to the Enter_Handler procedure to enter a handler.

   subtype Code_Loc is System.Address;
   --  Code location used in building exception tables and for call
   --  addresses when propagating an exception (also traceback table)
   --  Values of this type are created by using Label'Address or
   --  extracted from machine states using Get_Code_Loc.

   --------------------
   -- Handler_Record --
   --------------------

   --  A Handler record is built for each choice for each exception handler
   --  in a frame.

   function To_Exception_Id is
     new Unchecked_Conversion (SSL.Exception_Data_Ptr, AEX.Exception_Id);

   Others_Dummy_Exception : aliased SSL.Exception_Data;
   Others_Id : constant AEX.Exception_Id :=
                 To_Exception_Id (Others_Dummy_Exception'Access);
   --  Dummy exception used to signal others exception

   All_Others_Dummy_Exception : aliased SSL.Exception_Data;
   All_Others_Id : constant AEX.Exception_Id :=
                     To_Exception_Id (All_Others_Dummy_Exception'Access);
   --  Dummy exception used to signal all others exception (including
   --  exceptions not normally handled by others, e.g. Abort_Signal)

   type Handler_Record is record
      Lo : Code_Loc;
      Hi : Code_Loc;
      --  Range of PC values of code covered by this handler record. The
      --  handler covers all code addresses that are greater than the Lo
      --  value, and less than or equal to the Hi value.

      Id : AEX.Exception_Id;
      --  Id of exception being handled, or one of the above special values

      Handler : Handler_Loc;
      --  Address of label at start of handler
   end record;

   type Handler_Record_Ptr is access all Handler_Record;
   type Handler_Record_List is array (Natural range <>) of Handler_Record_Ptr;

   ---------------------------
   -- Subprogram_Descriptor --
   ---------------------------

   --  A Subprogram_Descriptor is built for each subprogram through which
   --  exceptions may propagate, this includes all Ada subprograms,
   --  and also all foreign language imported subprograms.

   subtype Subprogram_Info_Type is System.Address;
   --  This type is used to represent a value that is used to unwind stack
   --  frames. It references target dependent data that provides sufficient
   --  information (e.g. about the location of the return point, use of a
   --  frame pointer, save-over-call registers etc) to unwind the machine
   --  state to the caller. For some targets, this is simply a pointer to
   --  the entry point of the procedure (and the routine to pop the machine
   --  state disassembles the code at the entry point to obtain the required
   --  information). On other targets, it is a pointer to data created by the
   --  backend or assembler to represent the required information.

   No_Info : constant Subprogram_Info_Type := System.Null_Address;
   --  This is a special value used to indicate that it is not possible
   --  to pop past this frame. This is used at the outer level (e.g. for
   --  package elaboration procedures or the main procedure), and for any
   --  other foreign language procedure for which propagation is known
   --  to be impossible. An exception is considered unhandled if an
   --  attempt is made to pop a frame whose Subprogram_Info_Type value
   --  is set to No_Info.

   type Subprogram_Descriptor (Num_Handlers : Natural) is record
      Code : Code_Loc;
      --  This is a code location used to determine which procedure we are
      --  in. Most usually it is simply the entry address for the procedure.
      --  hA given address is considered to be within the procedure referenced
      --  by a Subprogram_Descriptor record if this is the descriptor for
      --  which the Code value is as large as possible without exceeding
      --  the given value.

      Subprogram_Info : Subprogram_Info_Type;
      --  This is a pointer to a target dependent data item that provides
      --  sufficient information for unwinding the stack frame of this
      --  procedure. A value of No_Info (zero) means that we are the
      --  outer level procedure.

      Handler_Records : Handler_Record_List (1 .. Num_Handlers);
      --  List of pointers to Handler_Records for this procedure. The array
      --  is sorted inside out, i.e. entries for inner frames appear before
      --  entries for outer handlers. This ensures that a serial search
      --  finds the innermost applicable handler
   end record;

   subtype Subprogram_Descriptor_0 is Subprogram_Descriptor (0);
   subtype Subprogram_Descriptor_1 is Subprogram_Descriptor (1);
   subtype Subprogram_Descriptor_2 is Subprogram_Descriptor (2);
   subtype Subprogram_Descriptor_3 is Subprogram_Descriptor (3);
   --  Predeclare commonly used subtypes for buildingt he tables

   type Subprogram_Descriptor_Ptr is access all Subprogram_Descriptor;

   type Subprogram_Descriptor_List
     is array (Natural range <>) of Subprogram_Descriptor_Ptr;

   type Subprogram_Descriptors_Record (Count : Natural) is record
      SDesc : Subprogram_Descriptor_List (1 .. Count);
   end record;

   type Subprogram_Descriptors_Ptr is
     access all Subprogram_Descriptors_Record;

   --------------------------
   -- Unit Exception_Table --
   --------------------------

   --  If a unit contains at least one subprogram, then a library level
   --  declaration of the form:

   --    Tnn : aliased constant Subprogram_Descriptors :=
   --            (Count => n,
   --             SDesc =>
   --              (SD1'Unrestricted_Access,
   --               SD2'Unrestricted_Access,
   --               ...
   --               SDn'Unrestricted_Access));
   --    pragma Export (Ada, Tnn, "__gnat_unit_name__SDP");

   --  is generated where the initializing expression is an array aggregate
   --  whose elements are pointers to the generated subprogram descriptors
   --  for the units.

   --  Note: the ALI file contains the designation UX in each unit entry
   --  if a unit exception table is generated.

   --  The binder generates a list of addresses of pointers to these tables.

end System.Exceptions;
