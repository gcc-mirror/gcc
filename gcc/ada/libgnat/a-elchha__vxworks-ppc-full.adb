------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2019, Free Software Foundation, Inc.         --
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

pragma Warnings (Off);
with System.Standard_Library;
pragma Warnings (On);

with GNAT.Debug_Utilities; use GNAT.Debug_Utilities;
with GNAT.IO; use GNAT.IO;

--  Default last chance handler for use with the full VxWorks 653 partition OS
--  Ada run-time library.

--  Logs error with health monitor, and dumps exception identity and argument
--  string for vxaddr2line for generation of a symbolic stack backtrace.

procedure Ada.Exceptions.Last_Chance_Handler (Except : Exception_Occurrence) is

   ----------------------
   -- APEX definitions --
   ----------------------

   pragma Warnings (Off);
   type Error_Code_Type is (
      Deadline_Missed,
      Application_Error,
      Numeric_Error,
      Illegal_Request,
      Stack_Overflow,
      Memory_Violation,
      Hardware_Fault,
      Power_Fail);
   pragma Warnings (On);
   pragma Convention (C, Error_Code_Type);
   --  APEX Health Management error codes

   type Message_Addr_Type is new System.Address;

   type Apex_Integer is range -(2 ** 31) .. (2 ** 31) - 1;
   pragma Convention (C, Apex_Integer);

   Max_Error_Message_Size : constant := 64;

   type Error_Message_Size_Type is new Apex_Integer range
      1 .. Max_Error_Message_Size;

   pragma Warnings (Off);
   type Return_Code_Type is (
      No_Error,        --  request valid and operation performed
      No_Action,       --  status of system unaffected by request
      Not_Available,   --  resource required by request unavailable
      Invalid_Param,   --  invalid parameter specified in request
      Invalid_Config,  --  parameter incompatible with configuration
      Invalid_Mode,    --  request incompatible with current mode
      Timed_Out);      --  time-out tied up with request has expired
   pragma Warnings (On);
   pragma Convention (C, Return_Code_Type);
   --  APEX return codes

   procedure Raise_Application_Error
     (Error_Code   : Error_Code_Type;
      Message_Addr : Message_Addr_Type;
      Length       : Error_Message_Size_Type;
      Return_Code  : out Return_Code_Type);
   pragma Import (C, Raise_Application_Error, "RAISE_APPLICATION_ERROR");

   procedure Unhandled_Terminate;
   pragma No_Return (Unhandled_Terminate);
   pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");
   --  Perform system dependent shutdown code

   procedure Adainit;
   pragma Import (Ada, Adainit, "adainit");

   Adainit_Addr : constant System.Address := Adainit'Code_Address;
   --  Part of arguments to vxaddr2line

   Result : Return_Code_Type;

   Message      : String :=
     Exception_Name (Except) &   ": " & ASCII.LF &
     Exception_Message (Except) & ASCII.NUL;

   Message_Length : Error_Message_Size_Type;

begin
   New_Line;
   Put_Line ("In last chance handler");
   Put_Line (Message (1 .. Message'Length - 1));
   New_Line;

   Put_Line ("adainit and traceback addresses for vxaddr2line:");

   Put (Image_C (Adainit_Addr)); Put (" ");

   for J in 1 .. Except.Num_Tracebacks loop
      Put (Image_C (Except.Tracebacks (J)));
      Put (" ");
   end loop;

   New_Line;

   if Message'Length > Error_Message_Size_Type'Last then
      Message_Length := Error_Message_Size_Type'Last;
   else
      Message_Length := Message'Length;
   end if;

   Raise_Application_Error
     (Error_Code   => Application_Error,
      Message_Addr => Message_Addr_Type (Message (1)'Address),
      Length       => Message_Length,
      Return_Code  => Result);

   --  Shutdown the run-time library now. The rest of the procedure needs to be
   --  careful not to use anything that would require runtime support. In
   --  particular, functions returning strings are banned since the sec stack
   --  is no longer functional.

   System.Standard_Library.Adafinal;
   Unhandled_Terminate;
end Ada.Exceptions.Last_Chance_Handler;
