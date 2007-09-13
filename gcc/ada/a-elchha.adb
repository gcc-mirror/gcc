------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2007, Free Software Foundation, Inc.         --
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

--  Default version for most targets

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

with System.Standard_Library; use System.Standard_Library;
--  Used for Adafinal

with System.Soft_Links;
--  Used for Task_Termination_Handler
--           Task_Termination_NT

procedure Ada.Exceptions.Last_Chance_Handler
  (Except : Exception_Occurrence)
is
   procedure Unhandled_Terminate;
   pragma No_Return (Unhandled_Terminate);
   pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");
   --  Perform system dependent shutdown code

   function Exception_Message_Length
     (X : Exception_Occurrence) return Natural;
   pragma Import (Ada, Exception_Message_Length, "__gnat_exception_msg_len");

   procedure Append_Info_Exception_Message
     (X : Exception_Occurrence; Info : in out String; Ptr : in out Natural);
   pragma Import
     (Ada, Append_Info_Exception_Message, "__gnat_append_info_e_msg");

   procedure Append_Info_Exception_Information
     (X : Exception_Occurrence; Info : in out String; Ptr : in out Natural);
   pragma Import
     (Ada, Append_Info_Exception_Information, "__gnat_append_info_e_info");

   procedure To_Stderr (S : String);
   pragma Import (Ada, To_Stderr, "__gnat_to_stderr");
   --  Little routine to output string to stderr

   Ptr   : Natural := 0;
   Nobuf : String (1 .. 0);

   Nline : constant String := String'(1 => ASCII.LF);
   --  Convenient shortcut

begin
   --  Do not execute any task termination code when shutting down the system.
   --  The Adafinal procedure would execute the task termination routine for
   --  normal termination, but we have already executed the task termination
   --  procedure because of an unhandled exception.

   System.Soft_Links.Task_Termination_Handler :=
     System.Soft_Links.Task_Termination_NT'Access;

   --  Let's shutdown the runtime now. The rest of the procedure needs to be
   --  careful not to use anything that would require runtime support. In
   --  particular, functions returning strings are banned since the sec stack
   --  is no longer functional. This is particularly important to note for the
   --  Exception_Information output. We used to allow the tailored version to
   --  show up here, which turned out to be a bad idea as it might involve a
   --  traceback decorator the length of which we don't control. Potentially
   --  heavy primary/secondary stack use or dynamic allocations right before
   --  this point are not welcome, moving the output before the finalization
   --  raises order of outputs concerns, and decorators are intended to only
   --  be used with exception traces, which should have been issued already.

   System.Standard_Library.Adafinal;

   --  Check for special case of raising _ABORT_SIGNAL, which is not
   --  really an exception at all. We recognize this by the fact that
   --  it is the only exception whose name starts with underscore.

   if To_Ptr (Except.Id.Full_Name) (1) = '_' then
      To_Stderr (Nline);
      To_Stderr ("Execution terminated by abort of environment task");
      To_Stderr (Nline);

   --  If no tracebacks, we print the unhandled exception in the old style
   --  (i.e. the style used before ZCX was implemented). We do this to
   --  retain compatibility.

   elsif Except.Num_Tracebacks = 0 then
      To_Stderr (Nline);
      To_Stderr ("raised ");
      To_Stderr
        (To_Ptr (Except.Id.Full_Name) (1 .. Except.Id.Name_Length - 1));

      if Exception_Message_Length (Except) /= 0 then
         To_Stderr (" : ");
         Append_Info_Exception_Message (Except, Nobuf, Ptr);
      end if;

      To_Stderr (Nline);

   --  Traceback exists

   else
      To_Stderr (Nline);
      To_Stderr ("Execution terminated by unhandled exception");
      To_Stderr (Nline);

      Append_Info_Exception_Information (Except, Nobuf, Ptr);
   end if;

   Unhandled_Terminate;
end Ada.Exceptions.Last_Chance_Handler;
