------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2003 Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  Default version for most targets

procedure Ada.Exceptions.Last_Chance_Handler
  (Except : Exception_Occurrence)
is
   procedure Unhandled_Terminate;
   pragma No_Return (Unhandled_Terminate);
   pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");
   --  Perform system dependent shutdown code

   function Tailored_Exception_Information
     (X : Exception_Occurrence) return String;
   --  Exception information to be output in the case of automatic tracing
   --  requested through GNAT.Exception_Traces.
   --
   --  This is the same as Exception_Information if no backtrace decorator
   --  is currently in place. Otherwise, this is Exception_Information with
   --  the call chain raw addresses replaced by the result of a call to the
   --  current decorator provided with the call chain addresses.

   pragma Import
     (Ada, Tailored_Exception_Information,
        "__gnat_tailored_exception_information");

   procedure Tailored_Exception_Information
     (X    : Exception_Occurrence;
      Buff : in out String;
      Last : in out Integer);
   --  Procedural version of the above function. Instead of returning the
   --  result, this one is put in Buff (Buff'first .. Buff'first + Last)

   procedure To_Stderr (S : String);
   pragma Import (Ada, To_Stderr, "__gnat_to_stderr");
   --  Little routine to output string to stderr

   Nline : constant String := String'(1 => ASCII.LF);
   --  Convenient shortcut

   Msg : constant String := Except.Msg (1 .. Except.Msg_Length);

   Max_Static_Exc_Info : constant := 1024;
   --  This should be enough for most exception information cases
   --  even though tailoring introduces some uncertainty.  The
   --  name+message should not exceed 320 chars, so that leaves at
   --  least 35 backtrace slots (each slot needs 19 chars for
   --  representing a 64 bit address).

   subtype Exc_Info_Type is String (1 .. Max_Static_Exc_Info);
   type Str_Ptr is access Exc_Info_Type;
   Exc_Info : Str_Ptr;
   Exc_Info_Last : Natural := 0;
   --  Buffer that is allocated to store the tailored exception
   --  information while Adafinal is run. This buffer is allocated
   --  on the heap only when it is needed. It is better to allocate
   --  on the heap than on the stack since stack overflows are more
   --  common than heap overflows.

   procedure Tailored_Exception_Information
     (X    : Exception_Occurrence;
      Buff : in out String;
      Last : in out Integer)
   is
      Info : constant String := Tailored_Exception_Information (X);
   begin
      Last := Info'Last;
      Buff (1 .. Last) := Info;
   end Tailored_Exception_Information;

begin
   --  First allocate & store the exception info in a buffer when
   --  we know it will be needed. This needs to be done before
   --  Adafinal because it implicitly uses the secondary stack.

   if Except.Id.Full_Name.all (1) /= '_'
     and then Except.Num_Tracebacks /= 0
   then
      Exc_Info := new Exc_Info_Type;
      if Exc_Info /= null then
         Tailored_Exception_Information
           (Except, Exc_Info.all, Exc_Info_Last);
      end if;
   end if;

   --  Let's shutdown the runtime now. The rest of the procedure
   --  needs to be careful not to use anything that would require
   --  runtime support. In particular, functions returning strings
   --  are banned since the sec stack is no longer functional.
   System.Standard_Library.Adafinal;

   --  Check for special case of raising _ABORT_SIGNAL, which is not
   --  really an exception at all. We recognize this by the fact that
   --  it is the only exception whose name starts with underscore.

   if Except.Id.Full_Name.all (1) = '_' then
      To_Stderr (Nline);
      To_Stderr ("Execution terminated by abort of environment task");
      To_Stderr (Nline);

   --  If no tracebacks, we print the unhandled exception in the old style
   --  (i.e. the style used before ZCX was implemented). We do this to
   --  retain compatibility.

   elsif Except.Num_Tracebacks = 0 then
      To_Stderr (Nline);
      To_Stderr ("raised ");
      To_Stderr (Except.Id.Full_Name.all (1 .. Except.Id.Name_Length - 1));

      if Msg'Length /= 0 then
         To_Stderr (" : ");
         To_Stderr (Msg);
      end if;

      To_Stderr (Nline);

   --  Traceback exists

   else
      --  Note we can have this whole information output twice if
      --  this occurrence gets reraised up to here.

      To_Stderr (Nline);
      To_Stderr ("Execution terminated by unhandled exception");
      To_Stderr (Nline);
      To_Stderr (Exc_Info (1 .. Exc_Info_Last));
   end if;

   Unhandled_Terminate;
end Ada.Exceptions.Last_Chance_Handler;
