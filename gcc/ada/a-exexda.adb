------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       ADA.EXCEPTIONS.EXCEPTION_DATA                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

with System.Storage_Elements; use System.Storage_Elements;

separate (Ada.Exceptions)
package body Exception_Data is

   --  This unit implements the Exception_Information related services for
   --  both the Ada standard requirements and the GNAT.Exception_Traces
   --  facility.

   --  There are common parts between the contents of Exception_Information
   --  (the regular Ada interface) and Tailored_Exception_Information (what
   --  the automatic backtracing output includes). The overall structure is
   --  sketched below:

   --
   --                      Exception_Information
   --                               |
   --                       +-------+--------+
   --                       |                |
   --                Basic_Exc_Info & Basic_Exc_Tback
   --                    (B_E_I)          (B_E_TB)

   --           o--
   --  (B_E_I)  |  Exception_Name: <exception name> (as in Exception_Name)
   --           |  Message: <message> (or a null line if no message)
   --           |  PID=nnnn (if != 0)
   --           o--
   --  (B_E_TB) |  Call stack traceback locations:
   --           |  <0xyyyyyyyy 0xyyyyyyyy ...>
   --           o--

   --                  Tailored_Exception_Information
   --                               |
   --                    +----------+----------+
   --                    |                     |
   --             Basic_Exc_Info    &  Tailored_Exc_Tback
   --                                          |
   --                              +-----------+------------+
   --                              |                        |
   --                       Basic_Exc_Tback    Or    Tback_Decorator
   --                     if no decorator set           otherwise

   --  Functions returning String imply secondary stack use, which is a heavy
   --  mechanism requiring run-time support. Besides, some of the routines we
   --  provide here are to be used by the default Last_Chance_Handler, at the
   --  critical point where the runtime is about to be finalized. Since most
   --  of the items we have at hand are of bounded length, we also provide a
   --  procedural interface able to incrementally append the necessary bits to
   --  a preallocated buffer or output them straight to stderr.

   --  The procedural interface is composed of two major sections: a neutral
   --  section for basic types like Address, Character, Natural or String, and
   --  an exception oriented section for the e.g. Basic_Exception_Information.
   --  This is the Append_Info family of procedures below.

   --  Output to stderr is commanded by passing an empty buffer to update, and
   --  care is taken not to overflow otherwise.

   --------------------------------------------
   -- Procedural Interface - Neutral section --
   --------------------------------------------

   procedure Append_Info_Address
     (A    : Address;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Character
     (C    : Character;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Nat
     (N    : Natural;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_NL
     (Info : in out String;
      Ptr  : in out Natural);
   pragma Inline (Append_Info_NL);

   procedure Append_Info_String
     (S    : String;
      Info : in out String;
      Ptr  : in out Natural);

   -------------------------------------------------------
   -- Procedural Interface - Exception oriented section --
   -------------------------------------------------------

   procedure Append_Info_Exception_Name
     (Id   : Exception_Id;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Exception_Name
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Exception_Message
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Basic_Exception_Information
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Basic_Exception_Traceback
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural);

   procedure Append_Info_Exception_Information
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural);


   --  The "functional" interface to the exception information not involving
   --  a traceback decorator uses preallocated intermediate buffers to avoid
   --  the use of secondary stack. Preallocation requires preliminary length
   --  computation, for which a series of functions are introduced:

   ---------------------------------
   -- Length evaluation utilities --
   ---------------------------------

   function Basic_Exception_Info_Maxlength
     (X : Exception_Occurrence) return Natural;

   function Basic_Exception_Tback_Maxlength
     (X : Exception_Occurrence) return Natural;

   function Exception_Info_Maxlength
     (X : Exception_Occurrence) return Natural;

   function Exception_Name_Length
     (Id : Exception_Id) return Natural;

   function Exception_Name_Length
     (X : Exception_Occurrence) return Natural;

   function Exception_Message_Length
     (X : Exception_Occurrence) return Natural;

   --------------------------
   -- Functional Interface --
   --------------------------

   function Basic_Exception_Traceback
     (X : Exception_Occurrence) return String;
   --  Returns an image of the complete call chain associated with an
   --  exception occurence in its most basic form, that is as a raw sequence
   --  of hexadecimal binary addresses.

   function Tailored_Exception_Traceback
     (X : Exception_Occurrence) return String;
   --  Returns an image of the complete call chain associated with an
   --  exception occurrence, either in its basic form if no decorator is
   --  in place, or as formatted by the decorator otherwise.

   -----------------------------------------------------------------------
   -- Services for the default Last_Chance_Handler and the task wrapper --
   -----------------------------------------------------------------------

   pragma Export
     (Ada, Append_Info_Exception_Message, "__gnat_append_info_e_msg");

   pragma Export
     (Ada, Append_Info_Exception_Information, "__gnat_append_info_e_info");

   pragma Export
     (Ada, Exception_Message_Length, "__gnat_exception_msg_len");

   -------------------------
   -- Append_Info_Address --
   -------------------------

   procedure Append_Info_Address
     (A    : Address;
      Info : in out String;
      Ptr  : in out Natural)
   is
      S : String (1 .. 18);
      P : Natural;
      N : Integer_Address;

      H : constant array (Integer range 0 .. 15) of Character :=
                                                         "0123456789abcdef";
   begin
      P := S'Last;
      N := To_Integer (A);
      loop
         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
         exit when N = 0;
      end loop;

      S (P - 1) := '0';
      S (P) := 'x';

      Append_Info_String (S (P - 1 .. S'Last), Info, Ptr);
   end Append_Info_Address;

   ---------------------------
   -- Append_Info_Character --
   ---------------------------

   procedure Append_Info_Character
     (C    : Character;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if Info'Length = 0 then
         To_Stderr (C);
      elsif Ptr < Info'Last then
         Ptr := Ptr + 1;
         Info (Ptr) := C;
      end if;
   end Append_Info_Character;

   ---------------------
   -- Append_Info_Nat --
   ---------------------

   procedure Append_Info_Nat
     (N    : Natural;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if N > 9 then
         Append_Info_Nat (N / 10, Info, Ptr);
      end if;

      Append_Info_Character
        (Character'Val (Character'Pos ('0') + N mod 10), Info, Ptr);
   end Append_Info_Nat;

   --------------------
   -- Append_Info_NL --
   --------------------

   procedure Append_Info_NL
     (Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      Append_Info_Character (ASCII.LF, Info, Ptr);
   end Append_Info_NL;

   ------------------------
   -- Append_Info_String --
   ------------------------

   procedure Append_Info_String
     (S    : String;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if Info'Length = 0 then
         To_Stderr (S);
      else
         declare
            Last : constant Natural :=
              Integer'Min (Ptr + S'Length, Info'Last);
         begin
            Info (Ptr + 1 .. Last) := S;
            Ptr := Last;
         end;
      end if;
   end Append_Info_String;

   ---------------------------------------------
   -- Append_Info_Basic_Exception_Information --
   ---------------------------------------------

   --  To ease the maximum length computation, we define and pull out a couple
   --  of string constants:

   BEI_Name_Header : constant String := "Exception name: ";
   BEI_Msg_Header  : constant String := "Message: ";
   BEI_PID_Header  : constant String := "PID: ";

   procedure Append_Info_Basic_Exception_Information
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural)
   is
      Name : String (1 .. Exception_Name_Length (X));
      --  Bufer in which to fetch the exception name, in order to check
      --  whether this is an internal _ABORT_SIGNAL or a regular occurrence.

      Name_Ptr : Natural := Name'First - 1;

   begin
      --  Output exception name and message except for _ABORT_SIGNAL, where
      --  these two lines are omitted.

      Append_Info_Exception_Name (X, Name, Name_Ptr);

      if Name (Name'First) /= '_' then
         Append_Info_String (BEI_Name_Header, Info, Ptr);
         Append_Info_String (Name, Info, Ptr);
         Append_Info_NL (Info, Ptr);

         if Exception_Message_Length (X) /= 0 then
            Append_Info_String (BEI_Msg_Header, Info, Ptr);
            Append_Info_Exception_Message  (X, Info, Ptr);
            Append_Info_NL (Info, Ptr);
         end if;
      end if;

      --  Output PID line if non-zero

      if X.Pid /= 0 then
         Append_Info_String (BEI_PID_Header, Info, Ptr);
         Append_Info_Nat (X.Pid, Info, Ptr);
         Append_Info_NL (Info, Ptr);
      end if;
   end Append_Info_Basic_Exception_Information;

   -------------------------------------------
   -- Basic_Exception_Information_Maxlength --
   -------------------------------------------

   function Basic_Exception_Info_Maxlength
     (X : Exception_Occurrence) return Natural is
   begin
      return
        BEI_Name_Header'Length + Exception_Name_Length (X) + 1
        + BEI_Msg_Header'Length + Exception_Message_Length (X) + 1
        + BEI_PID_Header'Length + 15;
   end Basic_Exception_Info_Maxlength;

   -------------------------------------------
   -- Append_Info_Basic_Exception_Traceback --
   -------------------------------------------

   --  As for Basic_Exception_Information:

   BETB_Header : constant String := "Call stack traceback locations:";

   procedure Append_Info_Basic_Exception_Traceback
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if X.Num_Tracebacks <= 0 then
         return;
      end if;

      Append_Info_String (BETB_Header, Info, Ptr);
      Append_Info_NL (Info, Ptr);

      for J in 1 .. X.Num_Tracebacks loop
         Append_Info_Address (TBE.PC_For (X.Tracebacks (J)), Info, Ptr);
         exit when J = X.Num_Tracebacks;
         Append_Info_Character (' ', Info, Ptr);
      end loop;

      Append_Info_NL (Info, Ptr);
   end Append_Info_Basic_Exception_Traceback;

   -----------------------------------------
   -- Basic_Exception_Traceback_Maxlength --
   -----------------------------------------

   function Basic_Exception_Tback_Maxlength
     (X : Exception_Occurrence) return Natural is
   begin
      return BETB_Header'Length + 1 + X.Num_Tracebacks * 19 + 1;
      --  19 =  2 + 16 + 1 for each address ("0x" + HHHH + " ")
   end Basic_Exception_Tback_Maxlength;

   ---------------------------------------
   -- Append_Info_Exception_Information --
   ---------------------------------------

   procedure Append_Info_Exception_Information
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      Append_Info_Basic_Exception_Information (X, Info, Ptr);
      Append_Info_Basic_Exception_Traceback   (X, Info, Ptr);
   end Append_Info_Exception_Information;

   ------------------------------
   -- Exception_Info_Maxlength --
   ------------------------------

   function Exception_Info_Maxlength
     (X : Exception_Occurrence) return Natural is
   begin
      return
        Basic_Exception_Info_Maxlength (X)
        + Basic_Exception_Tback_Maxlength (X);
   end Exception_Info_Maxlength;

   -----------------------------------
   -- Append_Info_Exception_Message --
   -----------------------------------

   procedure Append_Info_Exception_Message
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural) is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      declare
         Len : constant Natural := Exception_Message_Length (X);
         Msg : constant String (1 .. Len) := X.Msg (1 .. Len);
      begin
         Append_Info_String (Msg, Info, Ptr);
      end;
   end Append_Info_Exception_Message;

   --------------------------------
   -- Append_Info_Exception_Name --
   --------------------------------

   procedure Append_Info_Exception_Name
     (Id   : Exception_Id;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if Id = Null_Id then
         raise Constraint_Error;
      end if;

      declare
         Len  : constant Natural := Exception_Name_Length (Id);
         Name : constant String (1 .. Len) := Id.Full_Name (1 .. Len);
      begin
         Append_Info_String (Name, Info, Ptr);
      end;
   end Append_Info_Exception_Name;

   procedure Append_Info_Exception_Name
     (X    : Exception_Occurrence;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      Append_Info_Exception_Name (X.Id, Info, Ptr);
   end Append_Info_Exception_Name;

   ---------------------------
   -- Exception_Name_Length --
   ---------------------------

   function Exception_Name_Length
     (Id : Exception_Id) return Natural is
   begin
      --  What is stored in the internal Name buffer includes a terminating
      --  null character that we never care about.

      return Id.Name_Length - 1;
   end Exception_Name_Length;

   function Exception_Name_Length
     (X : Exception_Occurrence) return Natural is
   begin
      return Exception_Name_Length (X.Id);
   end Exception_Name_Length;

   ------------------------------
   -- Exception_Message_Length --
   ------------------------------

   function Exception_Message_Length
     (X : Exception_Occurrence) return Natural is
   begin
      return X.Msg_Length;
   end Exception_Message_Length;

   -------------------------------
   -- Basic_Exception_Traceback --
   -------------------------------

   function Basic_Exception_Traceback
     (X : Exception_Occurrence) return String
   is
      Info : aliased String (1 .. Basic_Exception_Tback_Maxlength (X));
      Ptr  : Natural := Info'First - 1;

   begin
      Append_Info_Basic_Exception_Traceback (X, Info, Ptr);
      return Info (Info'First .. Ptr);
   end Basic_Exception_Traceback;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information
     (X : Exception_Occurrence) return String
   is
      Info : String (1 .. Exception_Info_Maxlength (X));
      Ptr  : Natural := Info'First - 1;

   begin
      Append_Info_Exception_Information (X, Info, Ptr);
      return Info (Info'First .. Ptr);
   end Exception_Information;

   -------------------------
   -- Set_Exception_C_Msg --
   -------------------------

   procedure Set_Exception_C_Msg
     (Id   : Exception_Id;
      Msg1 : Big_String_Ptr;
      Line : Integer        := 0;
      Msg2 : Big_String_Ptr := null)
   is
      Excep  : constant EOA := Get_Current_Excep.all;
      Val    : Integer := Line;
      Remind : Integer;
      Size   : Integer := 1;
      Ptr    : Natural;

   begin
      Exception_Propagation.Setup_Exception (Excep, Excep);
      Excep.Exception_Raised := False;
      Excep.Id               := Id;
      Excep.Num_Tracebacks   := 0;
      Excep.Pid              := Local_Partition_ID;
      Excep.Msg_Length       := 0;
      Excep.Cleanup_Flag     := False;

      while Msg1 (Excep.Msg_Length + 1) /= ASCII.NUL
        and then Excep.Msg_Length < Exception_Msg_Max_Length
      loop
         Excep.Msg_Length := Excep.Msg_Length + 1;
         Excep.Msg (Excep.Msg_Length) := Msg1 (Excep.Msg_Length);
      end loop;

      --  Append line number if present

      if Line > 0 then

         --  Compute the number of needed characters

         while Val > 0 loop
            Val := Val / 10;
            Size := Size + 1;
         end loop;

         --  If enough characters are available, put the line number

         if Excep.Msg_Length <= Exception_Msg_Max_Length - Size then
            Excep.Msg (Excep.Msg_Length + 1) := ':';
            Excep.Msg_Length := Excep.Msg_Length + Size;
            Val := Line;
            Size := 0;

            while Val > 0 loop
               Remind := Val rem 10;
               Val := Val / 10;
               Excep.Msg (Excep.Msg_Length - Size) :=
                 Character'Val (Remind + Character'Pos ('0'));
               Size := Size + 1;
            end loop;
         end if;
      end if;

      --  Append second message if present

      if Msg2 /= null
        and then Excep.Msg_Length + 1 < Exception_Msg_Max_Length
      then
         Excep.Msg_Length := Excep.Msg_Length + 1;
         Excep.Msg (Excep.Msg_Length) := ' ';

         Ptr := 1;
         while Msg2 (Ptr) /= ASCII.NUL
           and then Excep.Msg_Length < Exception_Msg_Max_Length
         loop
            Excep.Msg_Length := Excep.Msg_Length + 1;
            Excep.Msg (Excep.Msg_Length) := Msg2 (Ptr);
            Ptr := Ptr + 1;
         end loop;
      end if;
   end Set_Exception_C_Msg;

   -----------------------
   -- Set_Exception_Msg --
   -----------------------

   procedure Set_Exception_Msg
     (Id      : Exception_Id;
      Message : String)
   is
      Len   : constant Natural :=
                Natural'Min (Message'Length, Exception_Msg_Max_Length);
      First : constant Integer := Message'First;
      Excep  : constant EOA := Get_Current_Excep.all;

   begin
      Exception_Propagation.Setup_Exception (Excep, Excep);
      Excep.Exception_Raised := False;
      Excep.Msg_Length       := Len;
      Excep.Msg (1 .. Len)   := Message (First .. First + Len - 1);
      Excep.Id               := Id;
      Excep.Num_Tracebacks   := 0;
      Excep.Pid              := Local_Partition_ID;
      Excep.Cleanup_Flag     := False;

   end Set_Exception_Msg;

   ----------------------------------
   -- Tailored_Exception_Traceback --
   ----------------------------------

   function Tailored_Exception_Traceback
     (X : Exception_Occurrence) return String
   is
      --  We reference the decorator *wrapper* here and not the decorator
      --  itself. The purpose of the local variable Wrapper is to prevent a
      --  potential race condition in the code below. The atomicity of this
      --  assignment is enforced by pragma Atomic in System.Soft_Links.

      --  The potential race condition here, if no local variable was used,
      --  relates to the test upon the wrapper's value and the call, which
      --  are not performed atomically. With the local variable, potential
      --  changes of the wrapper's global value between the test and the
      --  call become inoffensive.

      Wrapper : constant Traceback_Decorator_Wrapper_Call :=
                  Traceback_Decorator_Wrapper;

   begin
      if Wrapper = null then
         return Basic_Exception_Traceback (X);
      else
         return Wrapper.all (X.Tracebacks'Address, X.Num_Tracebacks);
      end if;
   end Tailored_Exception_Traceback;

   ------------------------------------
   -- Tailored_Exception_Information --
   ------------------------------------

   function Tailored_Exception_Information
     (X : Exception_Occurrence) return String
   is
      --  The tailored exception information is the basic information
      --  associated with the tailored call chain backtrace.

      Tback_Info : constant String  := Tailored_Exception_Traceback (X);
      Tback_Len  : constant Natural := Tback_Info'Length;

      Info : String (1 .. Basic_Exception_Info_Maxlength (X) + Tback_Len);
      Ptr  : Natural := Info'First - 1;

   begin
      Append_Info_Basic_Exception_Information (X, Info, Ptr);
      Append_Info_String (Tback_Info, Info, Ptr);
      return Info (Info'First .. Ptr);
   end Tailored_Exception_Information;

end Exception_Data;
