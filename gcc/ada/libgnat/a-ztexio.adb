------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                A D A . W I D E _ W I D E _ T E X T _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with Ada.Streams;          use Ada.Streams;
with Interfaces.C_Streams; use Interfaces.C_Streams;

with System.CRTL;
with System.File_IO;
with System.WCh_Cnv;       use System.WCh_Cnv;
with System.WCh_Con;       use System.WCh_Con;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

pragma Elaborate_All (System.File_IO);
--  Needed because of calls to Chain_File in package body elaboration

package body Ada.Wide_Wide_Text_IO is

   package FIO renames System.File_IO;

   subtype AP is FCB.AFCB_Ptr;

   function To_FCB is new Ada.Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_TIO is new Ada.Unchecked_Conversion (FCB.File_Mode, File_Mode);
   use type FCB.File_Mode;

   use type System.CRTL.size_t;

   WC_Encoding : constant Character;
   pragma Import (C, WC_Encoding, "__gl_wc_encoding");
   --  Default wide character encoding

   Err_Name : aliased String := "*stderr" & ASCII.NUL;
   In_Name  : aliased String := "*stdin" & ASCII.NUL;
   Out_Name : aliased String := "*stdout" & ASCII.NUL;
   --  Names of standard files
   --
   --  Use "preallocated" strings to avoid calling "new" during the elaboration
   --  of the run time. This is needed in the tasking case to avoid calling
   --  Task_Lock too early. A filename is expected to end with a null character
   --  in the runtime, here the null characters are added just to have a
   --  correct filename length.
   --
   --  Note: the names for these files are bogus, and probably it would be
   --  better for these files to have no names, but the ACVC tests insist.
   --  We use names that are bound to fail in open etc.

   Null_Str : aliased constant String := "";
   --  Used as form string for standard files

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_Wide_Wide_Char_Immed
     (C    : Character;
      File : File_Type) return Wide_Wide_Character;
   --  This routine is identical to Get_Wide_Wide_Char, except that the reads
   --  are done in Get_Immediate mode (i.e. without waiting for a line return).

   function Getc_Immed (File : File_Type) return int;
   --  This routine is identical to Getc, except that the read is done in
   --  Get_Immediate mode (i.e. without waiting for a line return).

   procedure Putc (ch : int; File : File_Type);
   --  Outputs the given character to the file, which has already been checked
   --  for being in output status. Device_Error is raised if the character
   --  cannot be written.

   procedure Set_WCEM (File : in out File_Type);
   --  Called by Open and Create to set the wide character encoding method for
   --  the file, processing a WCEM form parameter if one is present. File is
   --  IN OUT because it may be closed in case of an error.

   procedure Terminate_Line (File : File_Type);
   --  If the file is in Write_File or Append_File mode, and the current line
   --  is not terminated, then a line terminator is written using New_Line.
   --  Note that there is no Terminate_Page routine, because the page mark at
   --  the end of the file is implied if necessary.

   procedure Ungetc (ch : int; File : File_Type);
   --  Pushes back character into stream, using ungetc. The caller has checked
   --  that the file is in read status. Device_Error is raised if the character
   --  cannot be pushed back. An attempt to push back and end of file character
   --  (EOF) is ignored.

   -------------------
   -- AFCB_Allocate --
   -------------------

   function AFCB_Allocate
     (Control_Block : Wide_Wide_Text_AFCB) return FCB.AFCB_Ptr
   is
      pragma Unreferenced (Control_Block);
   begin
      return new Wide_Wide_Text_AFCB;
   end AFCB_Allocate;

   ----------------
   -- AFCB_Close --
   ----------------

   procedure AFCB_Close (File : not null access Wide_Wide_Text_AFCB) is
   begin
      --  If the file being closed is one of the current files, then close
      --  the corresponding current file. It is not clear that this action
      --  is required (RM A.10.3(23)) but it seems reasonable, and besides
      --  ACVC test CE3208A expects this behavior.

      if File = Current_In then
         Current_In := null;
      elsif File = Current_Out then
         Current_Out := null;
      elsif File = Current_Err then
         Current_Err := null;
      end if;

      Terminate_Line (File.all'Access);
   end AFCB_Close;

   ---------------
   -- AFCB_Free --
   ---------------

   procedure AFCB_Free (File : not null access Wide_Wide_Text_AFCB) is
      FT : File_Type := File.all'Access;

      procedure Free is new
        Ada.Unchecked_Deallocation (Wide_Wide_Text_AFCB, File_Type);

   begin
      Free (FT);
   end AFCB_Free;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File)'Unrestricted_Access);
   end Close;

   ---------
   -- Col --
   ---------

   --  Note: we assume that it is impossible in practice for the column
   --  to exceed the value of Count'Last, i.e. no check is required for
   --  overflow raising layout error.

   function Col (File : File_Type) return Positive_Count is
   begin
      FIO.Check_File_Open (AP (File));
      return File.Col;
   end Col;

   function Col return Positive_Count is
   begin
      return Col (Current_Out);
   end Col;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
      Dummy_File_Control_Block : Wide_Wide_Text_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'W',
                Creat     => True,
                Text      => True);

      File.Self := File;
      Set_WCEM (File);
   end Create;

   -------------------
   -- Current_Error --
   -------------------

   function Current_Error return File_Type is
   begin
      return Current_Err;
   end Current_Error;

   function Current_Error return File_Access is
   begin
      return Current_Err.Self'Access;
   end Current_Error;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input return File_Type is
   begin
      return Current_In;
   end Current_Input;

   function Current_Input return File_Access is
   begin
      return Current_In.Self'Access;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return File_Type is
   begin
      return Current_Out;
   end Current_Output;

   function Current_Output return File_Access is
   begin
      return Current_Out.Self'Access;
   end Current_Output;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
   begin
      FIO.Delete (AP (File)'Unrestricted_Access);
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_Wide_Wide_Character then
         return False;

      elsif File.Before_LM then
         if File.Before_LM_PM then
            return Nextc (File) = EOF;
         end if;

      else
         ch := Getc (File);

         if ch = EOF then
            return True;

         elsif ch /= LM then
            Ungetc (ch, File);
            return False;

         else -- ch = LM
            File.Before_LM := True;
         end if;
      end if;

      --  Here we are just past the line mark with Before_LM set so that we
      --  do not have to try to back up past the LM, thus avoiding the need
      --  to back up more than one character.

      ch := Getc (File);

      if ch = EOF then
         return True;

      elsif ch = PM and then File.Is_Regular_File then
         File.Before_LM_PM := True;
         return Nextc (File) = EOF;

      --  Here if neither EOF nor PM followed end of line

      else
         Ungetc (ch, File);
         return False;
      end if;

   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_In);
   end End_Of_File;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (File : File_Type) return Boolean is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_Wide_Wide_Character then
         return False;

      elsif File.Before_LM then
         return True;

      else
         ch := Getc (File);

         if ch = EOF then
            return True;

         else
            Ungetc (ch, File);
            return (ch = LM);
         end if;
      end if;
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return End_Of_Line (Current_In);
   end End_Of_Line;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page (File : File_Type) return Boolean is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if not File.Is_Regular_File then
         return False;

      elsif File.Before_Wide_Wide_Character then
         return False;

      elsif File.Before_LM then
         if File.Before_LM_PM then
            return True;
         end if;

      else
         ch := Getc (File);

         if ch = EOF then
            return True;

         elsif ch /= LM then
            Ungetc (ch, File);
            return False;

         else -- ch = LM
            File.Before_LM := True;
         end if;
      end if;

      --  Here we are just past the line mark with Before_LM set so that we
      --  do not have to try to back up past the LM, thus avoiding the need
      --  to back up more than one character.

      ch := Nextc (File);

      return ch = PM or else ch = EOF;
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return End_Of_Page (Current_In);
   end End_Of_Page;

   -----------
   -- Flush --
   -----------

   procedure Flush (File : File_Type) is
   begin
      FIO.Flush (AP (File));
   end Flush;

   procedure Flush is
   begin
      Flush (Current_Out);
   end Flush;

   ----------
   -- Form --
   ----------

   function Form (File : File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

   ---------
   -- Get --
   ---------

   procedure Get
     (File : File_Type;
      Item : out Wide_Wide_Character)
   is
      C : Character;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_Wide_Wide_Character then
         File.Before_Wide_Wide_Character := False;
         Item := File.Saved_Wide_Wide_Character;

      --  Ada.Text_IO checks Before_LM_PM here, shouldn't we do the same???

      else
         Get_Character (File, C);
         Item := Get_Wide_Wide_Char (C, File);
      end if;
   end Get;

   procedure Get (Item : out Wide_Wide_Character) is
   begin
      Get (Current_In, Item);
   end Get;

   procedure Get
     (File : File_Type;
      Item : out Wide_Wide_String)
   is
   begin
      for J in Item'Range loop
         Get (File, Item (J));
      end loop;
   end Get;

   procedure Get (Item : out Wide_Wide_String) is
   begin
      Get (Current_In, Item);
   end Get;

   -------------------
   -- Get_Character --
   -------------------

   procedure Get_Character
     (File : File_Type;
      Item : out Character)
   is
      ch : int;

   begin
      if File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;
         File.Col := 1;

         if File.Before_LM_PM then
            File.Line := 1;
            File.Page := File.Page + 1;
            File.Before_LM_PM := False;

         else
            File.Line := File.Line + 1;
         end if;
      end if;

      loop
         ch := Getc (File);

         if ch = EOF then
            raise End_Error;

         elsif ch = LM then
            File.Line := File.Line + 1;
            File.Col := 1;

         elsif ch = PM and then File.Is_Regular_File then
            File.Page := File.Page + 1;
            File.Line := 1;

         else
            Item := Character'Val (ch);
            File.Col := File.Col + 1;
            return;
         end if;
      end loop;
   end Get_Character;

   -------------------
   -- Get_Immediate --
   -------------------

   procedure Get_Immediate
     (File : File_Type;
      Item : out Wide_Wide_Character)
   is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_Wide_Wide_Character then
         File.Before_Wide_Wide_Character := False;
         Item := File.Saved_Wide_Wide_Character;

      elsif File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;
         Item := Wide_Wide_Character'Val (LM);

      else
         ch := Getc_Immed (File);

         if ch = EOF then
            raise End_Error;
         else
            Item := Get_Wide_Wide_Char_Immed (Character'Val (ch), File);
         end if;
      end if;
   end Get_Immediate;

   procedure Get_Immediate
     (Item : out Wide_Wide_Character)
   is
   begin
      Get_Immediate (Current_In, Item);
   end Get_Immediate;

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Wide_Wide_Character;
      Available : out Boolean)
   is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));
      Available := True;

      if File.Before_Wide_Wide_Character then
         File.Before_Wide_Wide_Character := False;
         Item := File.Saved_Wide_Wide_Character;

      elsif File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;
         Item := Wide_Wide_Character'Val (LM);

      else
         --  Shouldn't we use getc_immediate_nowait here, like Text_IO???

         ch := Getc_Immed (File);

         if ch = EOF then
            raise End_Error;
         else
            Item := Get_Wide_Wide_Char_Immed (Character'Val (ch), File);
         end if;
      end if;
   end Get_Immediate;

   procedure Get_Immediate
     (Item      : out Wide_Wide_Character;
      Available : out Boolean)
   is
   begin
      Get_Immediate (Current_In, Item, Available);
   end Get_Immediate;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_Wide_String;
      Last : out Natural)
   is
   begin
      FIO.Check_Read_Status (AP (File));
      Last := Item'First - 1;

      --  Immediate exit for null string, this is a case in which we do not
      --  need to test for end of file and we do not skip a line mark under
      --  any circumstances.

      if Last >= Item'Last then
         return;
      end if;

      --  Here we have at least one character, if we are immediately before
      --  a line mark, then we will just skip past it storing no characters.

      if File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;

      --  Otherwise we need to read some characters

      else
         --  If we are at the end of file now, it means we are trying to
         --  skip a file terminator and we raise End_Error (RM A.10.7(20))

         if Nextc (File) = EOF then
            raise End_Error;
         end if;

         --  Loop through characters in string

         loop
            --  Exit the loop if read is terminated by encountering line mark
            --  Note that the use of Skip_Line here ensures we properly deal
            --  with setting the page and line numbers.

            if End_Of_Line (File) then
               Skip_Line (File);
               return;
            end if;

            --  Otherwise store the character, note that we know that ch is
            --  something other than LM or EOF. It could possibly be a page
            --  mark if there is a stray page mark in the middle of a line,
            --  but this is not an official page mark in any case, since
            --  official page marks can only follow a line mark. The whole
            --  page business is pretty much nonsense anyway, so we do not
            --  want to waste time trying to make sense out of non-standard
            --  page marks in the file. This means that the behavior of
            --  Get_Line is different from repeated Get of a character, but
            --  that's too bad. We only promise that page numbers etc make
            --  sense if the file is formatted in a standard manner.

            --  Note: we do not adjust the column number because it is quicker
            --  to adjust it once at the end of the operation than incrementing
            --  it each time around the loop.

            Last := Last + 1;
            Get (File, Item (Last));

            --  All done if the string is full, this is the case in which
            --  we do not skip the following line mark. We need to adjust
            --  the column number in this case.

            if Last = Item'Last then
               File.Col := File.Col + Count (Item'Length);
               return;
            end if;

            --  Exit from the loop if we are at the end of file. This happens
            --  if we have a last line that is not terminated with a line mark.
            --  In this case we consider that there is an implied line mark;
            --  this is a non-standard file, but we will treat it nicely.

            exit when Nextc (File) = EOF;
         end loop;
      end if;
   end Get_Line;

   procedure Get_Line
     (Item : out Wide_Wide_String;
      Last : out Natural)
   is
   begin
      Get_Line (Current_In, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return Wide_Wide_String is
      Buffer : Wide_Wide_String (1 .. 500);
      Last   : Natural;

      function Get_Rest (S : Wide_Wide_String) return Wide_Wide_String;
      --  This is a recursive function that reads the rest of the line and
      --  returns it. S is the part read so far.

      --------------
      -- Get_Rest --
      --------------

      function Get_Rest (S : Wide_Wide_String) return Wide_Wide_String is

         --  Each time we allocate a buffer the same size as what we have
         --  read so far. This limits us to a logarithmic number of calls
         --  to Get_Rest and also ensures only a linear use of stack space.

         Buffer : Wide_Wide_String (1 .. S'Length);
         Last   : Natural;

      begin
         Get_Line (File, Buffer, Last);

         declare
            R : constant Wide_Wide_String := S & Buffer (1 .. Last);
         begin
            if Last < Buffer'Last then
               return R;
            else
               return Get_Rest (R);
            end if;
         end;
      end Get_Rest;

   --  Start of processing for Get_Line

   begin
      Get_Line (File, Buffer, Last);

      if Last < Buffer'Last then
         return Buffer (1 .. Last);
      else
         return Get_Rest (Buffer (1 .. Last));
      end if;
   end Get_Line;

   function Get_Line return Wide_Wide_String is
   begin
      return Get_Line (Current_In);
   end Get_Line;

   ------------------------
   -- Get_Wide_Wide_Char --
   ------------------------

   function Get_Wide_Wide_Char
     (C    : Character;
      File : File_Type) return Wide_Wide_Character
   is
      function In_Char return Character;
      --  Function used to obtain additional characters it the wide character
      --  sequence is more than one character long.

      function WC_In is new Char_Sequence_To_UTF_32 (In_Char);

      -------------
      -- In_Char --
      -------------

      function In_Char return Character is
         ch : constant Integer := Getc (File);
      begin
         if ch = EOF then
            raise End_Error;
         else
            return Character'Val (ch);
         end if;
      end In_Char;

   --  Start of processing for Get_Wide_Wide_Char

   begin
      FIO.Check_Read_Status (AP (File));
      return Wide_Wide_Character'Val (WC_In (C, File.WC_Method));
   end Get_Wide_Wide_Char;

   ------------------------------
   -- Get_Wide_Wide_Char_Immed --
   ------------------------------

   function Get_Wide_Wide_Char_Immed
     (C    : Character;
      File : File_Type) return Wide_Wide_Character
   is
      function In_Char return Character;
      --  Function used to obtain additional characters it the wide character
      --  sequence is more than one character long.

      function WC_In is new Char_Sequence_To_UTF_32 (In_Char);

      -------------
      -- In_Char --
      -------------

      function In_Char return Character is
         ch : constant Integer := Getc_Immed (File);
      begin
         if ch = EOF then
            raise End_Error;
         else
            return Character'Val (ch);
         end if;
      end In_Char;

   --  Start of processing for Get_Wide_Wide_Char_Immed

   begin
      FIO.Check_Read_Status (AP (File));
      return Wide_Wide_Character'Val (WC_In (C, File.WC_Method));
   end Get_Wide_Wide_Char_Immed;

   ----------
   -- Getc --
   ----------

   function Getc (File : File_Type) return int is
      ch : int;

   begin
      ch := fgetc (File.Stream);

      if ch = EOF and then ferror (File.Stream) /= 0 then
         raise Device_Error;
      else
         return ch;
      end if;
   end Getc;

   ----------------
   -- Getc_Immed --
   ----------------

   function Getc_Immed (File : File_Type) return int is
      ch          : int;
      end_of_file : int;

      procedure getc_immediate
        (stream : FILEs; ch : out int; end_of_file : out int);
      pragma Import (C, getc_immediate, "getc_immediate");

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;
         ch := LM;

      else
         getc_immediate (File.Stream, ch, end_of_file);

         if ferror (File.Stream) /= 0 then
            raise Device_Error;
         elsif end_of_file /= 0 then
            return EOF;
         end if;
      end if;

      return ch;
   end Getc_Immed;

   -------------------------------
   -- Initialize_Standard_Files --
   -------------------------------

   procedure Initialize_Standard_Files is
   begin
      Standard_Err.Stream            := stderr;
      Standard_Err.Name              := Err_Name'Access;
      Standard_Err.Form              := Null_Str'Unrestricted_Access;
      Standard_Err.Mode              := FCB.Out_File;
      Standard_Err.Is_Regular_File   := is_regular_file (fileno (stderr)) /= 0;
      Standard_Err.Is_Temporary_File := False;
      Standard_Err.Is_System_File    := True;
      Standard_Err.Text_Encoding     := Default_Text;
      Standard_Err.Access_Method     := 'T';
      Standard_Err.Self              := Standard_Err;
      Standard_Err.WC_Method         := Default_WCEM;

      Standard_In.Stream             := stdin;
      Standard_In.Name               := In_Name'Access;
      Standard_In.Form               := Null_Str'Unrestricted_Access;
      Standard_In.Mode               := FCB.In_File;
      Standard_In.Is_Regular_File    := is_regular_file (fileno (stdin)) /= 0;
      Standard_In.Is_Temporary_File  := False;
      Standard_In.Is_System_File     := True;
      Standard_In.Text_Encoding      := Default_Text;
      Standard_In.Access_Method      := 'T';
      Standard_In.Self               := Standard_In;
      Standard_In.WC_Method          := Default_WCEM;

      Standard_Out.Stream            := stdout;
      Standard_Out.Name              := Out_Name'Access;
      Standard_Out.Form              := Null_Str'Unrestricted_Access;
      Standard_Out.Mode              := FCB.Out_File;
      Standard_Out.Is_Regular_File   := is_regular_file (fileno (stdout)) /= 0;
      Standard_Out.Is_Temporary_File := False;
      Standard_Out.Is_System_File    := True;
      Standard_Out.Text_Encoding     := Default_Text;
      Standard_Out.Access_Method     := 'T';
      Standard_Out.Self              := Standard_Out;
      Standard_Out.WC_Method         := Default_WCEM;

      FIO.Make_Unbuffered (AP (Standard_Out));
      FIO.Make_Unbuffered (AP (Standard_Err));
   end Initialize_Standard_Files;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : File_Type) return Boolean is
   begin
      return FIO.Is_Open (AP (File));
   end Is_Open;

   ----------
   -- Line --
   ----------

   --  Note: we assume that it is impossible in practice for the line
   --  to exceed the value of Count'Last, i.e. no check is required for
   --  overflow raising layout error.

   function Line (File : File_Type) return Positive_Count is
   begin
      FIO.Check_File_Open (AP (File));
      return File.Line;
   end Line;

   function Line return Positive_Count is
   begin
      return Line (Current_Out);
   end Line;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (File : File_Type) return Count is
   begin
      FIO.Check_Write_Status (AP (File));
      return File.Line_Length;
   end Line_Length;

   function Line_Length return Count is
   begin
      return Line_Length (Current_Out);
   end Line_Length;

   ----------------
   -- Look_Ahead --
   ----------------

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Wide_Wide_Character;
      End_Of_Line : out Boolean)
   is
      ch : int;

   --  Start of processing for Look_Ahead

   begin
      FIO.Check_Read_Status (AP (File));

      --  If we are logically before a line mark, we can return immediately

      if File.Before_LM then
         End_Of_Line := True;
         Item := Wide_Wide_Character'Val (0);

      --  If we are before a wide character, just return it (this can happen
      --  if there are two calls to Look_Ahead in a row).

      elsif File.Before_Wide_Wide_Character then
         End_Of_Line := False;
         Item := File.Saved_Wide_Wide_Character;

      --  otherwise we must read a character from the input stream

      else
         ch := Getc (File);

         if ch = LM
           or else ch = EOF
           or else (ch = EOF and then File.Is_Regular_File)
         then
            End_Of_Line := True;
            Ungetc (ch, File);
            Item := Wide_Wide_Character'Val (0);

         --  Case where character obtained does not represent the start of an
         --  encoded sequence so it stands for itself and we can unget it with
         --  no difficulty.

         elsif not Is_Start_Of_Encoding
                     (Character'Val (ch), File.WC_Method)
         then
            End_Of_Line := False;
            Ungetc (ch, File);
            Item := Wide_Wide_Character'Val (ch);

         --  For the start of an encoding, we read the character using the
         --  Get_Wide_Wide_Char routine. It will occupy more than one byte so
         --  we can't put it back with ungetc. Instead we save it in the
         --  control block, setting a flag that everyone interested in reading
         --  characters must test before reading the stream.

         else
            Item := Get_Wide_Wide_Char (Character'Val (ch), File);
            End_Of_Line := False;
            File.Saved_Wide_Wide_Character := Item;
            File.Before_Wide_Wide_Character := True;
         end if;
      end if;
   end Look_Ahead;

   procedure Look_Ahead
     (Item        : out Wide_Wide_Character;
      End_Of_Line : out Boolean)
   is
   begin
      Look_Ahead (Current_In, Item, End_Of_Line);
   end Look_Ahead;

   ----------
   -- Mode --
   ----------

   function Mode (File : File_Type) return File_Mode is
   begin
      return To_TIO (FIO.Mode (AP (File)));
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : File_Type) return String is
   begin
      return FIO.Name (AP (File));
   end Name;

   --------------
   -- New_Line --
   --------------

   procedure New_Line
     (File    : File_Type;
      Spacing : Positive_Count := 1)
   is
   begin
      --  Raise Constraint_Error if out of range value. The reason for this
      --  explicit test is that we don't want junk values around, even if
      --  checks are off in the caller.

      if not Spacing'Valid then
         raise Constraint_Error;
      end if;

      FIO.Check_Write_Status (AP (File));

      for K in 1 .. Spacing loop
         Putc (LM, File);
         File.Line := File.Line + 1;

         if File.Page_Length /= 0
           and then File.Line > File.Page_Length
         then
            Putc (PM, File);
            File.Line := 1;
            File.Page := File.Page + 1;
         end if;
      end loop;

      File.Col := 1;
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Current_Out, Spacing);
   end New_Line;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (File : File_Type) is
   begin
      FIO.Check_Write_Status (AP (File));

      if File.Col /= 1 or else File.Line = 1 then
         Putc (LM, File);
      end if;

      Putc (PM, File);
      File.Page := File.Page + 1;
      File.Line := 1;
      File.Col := 1;
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Current_Out);
   end New_Page;

   -----------
   -- Nextc --
   -----------

   function Nextc (File : File_Type) return int is
      ch : int;

   begin
      ch := fgetc (File.Stream);

      if ch = EOF then
         if ferror (File.Stream) /= 0 then
            raise Device_Error;
         end if;

      else
         if ungetc (ch, File.Stream) = EOF then
            raise Device_Error;
         end if;
      end if;

      return ch;
   end Nextc;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
      Dummy_File_Control_Block : Wide_Wide_Text_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'W',
                Creat     => False,
                Text      => True);

      File.Self := File;
      Set_WCEM (File);
   end Open;

   ----------
   -- Page --
   ----------

   --  Note: we assume that it is impossible in practice for the page
   --  to exceed the value of Count'Last, i.e. no check is required for
   --  overflow raising layout error.

   function Page (File : File_Type) return Positive_Count is
   begin
      FIO.Check_File_Open (AP (File));
      return File.Page;
   end Page;

   function Page return Positive_Count is
   begin
      return Page (Current_Out);
   end Page;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length (File : File_Type) return Count is
   begin
      FIO.Check_Write_Status (AP (File));
      return File.Page_Length;
   end Page_Length;

   function Page_Length return Count is
   begin
      return Page_Length (Current_Out);
   end Page_Length;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Wide_Wide_Character)
   is
      procedure Out_Char (C : Character);
      --  Procedure to output one character of a wide character sequence

      procedure WC_Out is new UTF_32_To_Char_Sequence (Out_Char);

      --------------
      -- Out_Char --
      --------------

      procedure Out_Char (C : Character) is
      begin
         Putc (Character'Pos (C), File);
      end Out_Char;

   --  Start of processing for Put

   begin
      FIO.Check_Write_Status (AP (File));
      WC_Out (Wide_Wide_Character'Pos (Item), File.WC_Method);
      File.Col := File.Col + 1;
   end Put;

   procedure Put (Item : Wide_Wide_Character) is
   begin
      Put (Current_Out, Item);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Wide_Wide_String)
   is
   begin
      for J in Item'Range loop
         Put (File, Item (J));
      end loop;
   end Put;

   procedure Put (Item : Wide_Wide_String) is
   begin
      Put (Current_Out, Item);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File : File_Type;
      Item : Wide_Wide_String)
   is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   procedure Put_Line (Item : Wide_Wide_String) is
   begin
      Put (Current_Out, Item);
      New_Line (Current_Out);
   end Put_Line;

   ----------
   -- Putc --
   ----------

   procedure Putc (ch : int; File : File_Type) is
   begin
      if fputc (ch, File.Stream) = EOF then
         raise Device_Error;
      end if;
   end Putc;

   ----------
   -- Read --
   ----------

   --  This is the primitive Stream Read routine, used when a Text_IO file
   --  is treated directly as a stream using Text_IO.Streams.Stream.

   procedure Read
     (File : in out Wide_Wide_Text_AFCB;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Discard_ch : int;
      pragma Unreferenced (Discard_ch);

   begin
      --  Need to deal with Before_Wide_Wide_Character ???

      if File.Mode /= FCB.In_File then
         raise Mode_Error;
      end if;

      --  Deal with case where our logical and physical position do not match
      --  because of being after an LM or LM-PM sequence when in fact we are
      --  logically positioned before it.

      if File.Before_LM then

         --  If we are before a PM, then it is possible for a stream read
         --  to leave us after the LM and before the PM, which is a bit
         --  odd. The easiest way to deal with this is to unget the PM,
         --  so we are indeed positioned between the characters. This way
         --  further stream read operations will work correctly, and the
         --  effect on text processing is a little weird, but what can
         --  be expected if stream and text input are mixed this way?

         if File.Before_LM_PM then
            Discard_ch := ungetc (PM, File.Stream);
            File.Before_LM_PM := False;
         end if;

         File.Before_LM := False;

         Item (Item'First) := Stream_Element (Character'Pos (ASCII.LF));

         if Item'Length = 1 then
            Last := Item'Last;

         else
            Last :=
              Item'First +
                Stream_Element_Offset
                  (fread (buffer => Item'Address,
                          index  => size_t (Item'First + 1),
                          size   => 1,
                          count  => Item'Length - 1,
                          stream => File.Stream));
         end if;

         return;
      end if;

      --  Now we do the read. Since this is a text file, it is normally in
      --  text mode, but stream data must be read in binary mode, so we
      --  temporarily set binary mode for the read, resetting it after.
      --  These calls have no effect in a system (like Unix) where there is
      --  no distinction between text and binary files.

      set_binary_mode (fileno (File.Stream));

      Last :=
        Item'First +
          Stream_Element_Offset
            (fread (Item'Address, 1, Item'Length, File.Stream)) - 1;

      if Last < Item'Last then
         if ferror (File.Stream) /= 0 then
            raise Device_Error;
         end if;
      end if;

      set_text_mode (fileno (File.Stream));
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (File : in out File_Type;
      Mode : File_Mode)
   is
   begin
      --  Don't allow change of mode for current file (RM A.10.2(5))

      if (File = Current_In or else
          File = Current_Out  or else
          File = Current_Error)
        and then To_FCB (Mode) /= File.Mode
      then
         raise Mode_Error;
      end if;

      Terminate_Line (File);
      FIO.Reset (AP (File)'Unrestricted_Access, To_FCB (Mode));
      File.Page := 1;
      File.Line := 1;
      File.Col  := 1;
      File.Line_Length := 0;
      File.Page_Length := 0;
      File.Before_LM := False;
      File.Before_LM_PM := False;
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Terminate_Line (File);
      FIO.Reset (AP (File)'Unrestricted_Access);
      File.Page := 1;
      File.Line := 1;
      File.Col  := 1;
      File.Line_Length := 0;
      File.Page_Length := 0;
      File.Before_LM := False;
      File.Before_LM_PM := False;
   end Reset;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col
     (File : File_Type;
      To   : Positive_Count)
   is
      ch : int;

   begin
      --  Raise Constraint_Error if out of range value. The reason for this
      --  explicit test is that we don't want junk values around, even if
      --  checks are off in the caller.

      if not To'Valid then
         raise Constraint_Error;
      end if;

      FIO.Check_File_Open (AP (File));

      if To = File.Col then
         return;
      end if;

      if Mode (File) >= Out_File then
         if File.Line_Length /= 0 and then To > File.Line_Length then
            raise Layout_Error;
         end if;

         if To < File.Col then
            New_Line (File);
         end if;

         while File.Col < To loop
            Put (File, ' ');
         end loop;

      else
         loop
            ch := Getc (File);

            if ch = EOF then
               raise End_Error;

            elsif ch = LM then
               File.Line := File.Line + 1;
               File.Col := 1;

            elsif ch = PM and then File.Is_Regular_File then
               File.Page := File.Page + 1;
               File.Line := 1;
               File.Col := 1;

            elsif To = File.Col then
               Ungetc (ch, File);
               return;

            else
               File.Col := File.Col + 1;
            end if;
         end loop;
      end if;
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Current_Out, To);
   end Set_Col;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (File : File_Type) is
   begin
      FIO.Check_Write_Status (AP (File));
      Current_Err := File;
   end Set_Error;

   ---------------
   -- Set_Input --
   ---------------

   procedure Set_Input (File : File_Type) is
   begin
      FIO.Check_Read_Status (AP (File));
      Current_In := File;
   end Set_Input;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (File : File_Type;
      To   : Positive_Count)
   is
   begin
      --  Raise Constraint_Error if out of range value. The reason for this
      --  explicit test is that we don't want junk values around, even if
      --  checks are off in the caller.

      if not To'Valid then
         raise Constraint_Error;
      end if;

      FIO.Check_File_Open (AP (File));

      if To = File.Line then
         return;
      end if;

      if Mode (File) >= Out_File then
         if File.Page_Length /= 0 and then To > File.Page_Length then
            raise Layout_Error;
         end if;

         if To < File.Line then
            New_Page (File);
         end if;

         while File.Line < To loop
            New_Line (File);
         end loop;

      else
         while To /= File.Line loop
            Skip_Line (File);
         end loop;
      end if;
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Current_Out, To);
   end Set_Line;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (File : File_Type; To : Count) is
   begin
      --  Raise Constraint_Error if out of range value. The reason for this
      --  explicit test is that we don't want junk values around, even if
      --  checks are off in the caller.

      if not To'Valid then
         raise Constraint_Error;
      end if;

      FIO.Check_Write_Status (AP (File));
      File.Line_Length := To;
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Set_Line_Length (Current_Out, To);
   end Set_Line_Length;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : File_Type) is
   begin
      FIO.Check_Write_Status (AP (File));
      Current_Out := File;
   end Set_Output;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (File : File_Type; To : Count) is
   begin
      --  Raise Constraint_Error if out of range value. The reason for this
      --  explicit test is that we don't want junk values around, even if
      --  checks are off in the caller.

      if not To'Valid then
         raise Constraint_Error;
      end if;

      FIO.Check_Write_Status (AP (File));
      File.Page_Length := To;
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Current_Out, To);
   end Set_Page_Length;

   --------------
   -- Set_WCEM --
   --------------

   procedure Set_WCEM (File : in out File_Type) is
      Start : Natural;
      Stop  : Natural;

   begin
      FIO.Form_Parameter (File.Form.all, "wcem", Start, Stop);

      if Start = 0 then
         File.WC_Method := Default_WCEM;

      else
         if Stop = Start then
            for J in WC_Encoding_Letters'Range loop
               if File.Form (Start) = WC_Encoding_Letters (J) then
                  File.WC_Method := J;
                  return;
               end if;
            end loop;
         end if;

         Close (File);
         raise Use_Error with "invalid WCEM form parameter";
      end if;
   end Set_WCEM;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line
     (File    : File_Type;
      Spacing : Positive_Count := 1)
   is
      ch : int;

   begin
      --  Raise Constraint_Error if out of range value. The reason for this
      --  explicit test is that we don't want junk values around, even if
      --  checks are off in the caller.

      if not Spacing'Valid then
         raise Constraint_Error;
      end if;

      FIO.Check_Read_Status (AP (File));

      for L in 1 .. Spacing loop
         if File.Before_LM then
            File.Before_LM := False;
            File.Before_LM_PM := False;

         else
            ch := Getc (File);

            --  If at end of file now, then immediately raise End_Error. Note
            --  that we can never be positioned between a line mark and a page
            --  mark, so if we are at the end of file, we cannot logically be
            --  before the implicit page mark that is at the end of the file.

            --  For the same reason, we do not need an explicit check for a
            --  page mark. If there is a FF in the middle of a line, the file
            --  is not in canonical format and we do not care about the page
            --  numbers for files other than ones in canonical format.

            if ch = EOF then
               raise End_Error;
            end if;

            --  If not at end of file, then loop till we get to an LM or EOF.
            --  The latter case happens only in non-canonical files where the
            --  last line is not terminated by LM, but we don't want to blow
            --  up for such files, so we assume an implicit LM in this case.

            loop
               exit when ch = LM or else ch = EOF;
               ch := Getc (File);
            end loop;
         end if;

         --  We have got past a line mark, now, for a regular file only,
         --  see if a page mark immediately follows this line mark and
         --  if so, skip past the page mark as well. We do not do this
         --  for non-regular files, since it would cause an undesirable
         --  wait for an additional character.

         File.Col := 1;
         File.Line := File.Line + 1;

         if File.Before_LM_PM then
            File.Page := File.Page + 1;
            File.Line := 1;
            File.Before_LM_PM := False;

         elsif File.Is_Regular_File then
            ch := Getc (File);

            --  Page mark can be explicit, or implied at the end of the file

            if (ch = PM or else ch = EOF)
              and then File.Is_Regular_File
            then
               File.Page := File.Page + 1;
               File.Line := 1;
            else
               Ungetc (ch, File);
            end if;
         end if;
      end loop;

      File.Before_Wide_Wide_Character := False;
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Current_In, Spacing);
   end Skip_Line;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page (File : File_Type) is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      --  If at page mark already, just skip it

      if File.Before_LM_PM then
         File.Before_LM := False;
         File.Before_LM_PM := False;
         File.Page := File.Page + 1;
         File.Line := 1;
         File.Col  := 1;
         return;
      end if;

      --  This is a bit tricky, if we are logically before an LM then
      --  it is not an error if we are at an end of file now, since we
      --  are not really at it.

      if File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;
         ch := Getc (File);

      --  Otherwise we do raise End_Error if we are at the end of file now

      else
         ch := Getc (File);

         if ch = EOF then
            raise End_Error;
         end if;
      end if;

      --  Now we can just rumble along to the next page mark, or to the
      --  end of file, if that comes first. The latter case happens when
      --  the page mark is implied at the end of file.

      loop
         exit when ch = EOF
           or else (ch = PM and then File.Is_Regular_File);
         ch := Getc (File);
      end loop;

      File.Page := File.Page + 1;
      File.Line := 1;
      File.Col  := 1;
      File.Before_Wide_Wide_Character := False;
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Current_In);
   end Skip_Page;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Standard_Err;
   end Standard_Error;

   function Standard_Error return File_Access is
   begin
      return Standard_Err'Access;
   end Standard_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Type is
   begin
      return Standard_In;
   end Standard_Input;

   function Standard_Input return File_Access is
   begin
      return Standard_In'Access;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Standard_Out;
   end Standard_Output;

   function Standard_Output return File_Access is
   begin
      return Standard_Out'Access;
   end Standard_Output;

   --------------------
   -- Terminate_Line --
   --------------------

   procedure Terminate_Line (File : File_Type) is
   begin
      FIO.Check_File_Open (AP (File));

      --  For file other than In_File, test for needing to terminate last line

      if Mode (File) /= In_File then

         --  If not at start of line definition need new line

         if File.Col /= 1 then
            New_Line (File);

         --  For files other than standard error and standard output, we
         --  make sure that an empty file has a single line feed, so that
         --  it is properly formatted. We avoid this for the standard files
         --  because it is too much of a nuisance to have these odd line
         --  feeds when nothing has been written to the file.

         elsif (File /= Standard_Err and then File /= Standard_Out)
           and then (File.Line = 1 and then File.Page = 1)
         then
            New_Line (File);
         end if;
      end if;
   end Terminate_Line;

   ------------
   -- Ungetc --
   ------------

   procedure Ungetc (ch : int; File : File_Type) is
   begin
      if ch /= EOF then
         if ungetc (ch, File.Stream) = EOF then
            raise Device_Error;
         end if;
      end if;
   end Ungetc;

   -----------
   -- Write --
   -----------

   --  This is the primitive Stream Write routine, used when a Text_IO file
   --  is treated directly as a stream using Text_IO.Streams.Stream.

   procedure Write
     (File : in out Wide_Wide_Text_AFCB;
      Item : Stream_Element_Array)
   is
      pragma Warnings (Off, File);
      --  Because in this implementation we don't need IN OUT, we only read

      Siz : constant size_t := Item'Length;

   begin
      if File.Mode = FCB.In_File then
         raise Mode_Error;
      end if;

      --  Now we do the write. Since this is a text file, it is normally in
      --  text mode, but stream data must be written in binary mode, so we
      --  temporarily set binary mode for the write, resetting it after.
      --  These calls have no effect in a system (like Unix) where there is
      --  no distinction between text and binary files.

      set_binary_mode (fileno (File.Stream));

      if fwrite (Item'Address, 1, Siz, File.Stream) /= Siz then
         raise Device_Error;
      end if;

      set_text_mode (fileno (File.Stream));
   end Write;

begin
   --  Initialize Standard Files

   for J in WC_Encoding_Method loop
      if WC_Encoding = WC_Encoding_Letters (J) then
         Default_WCEM := J;
      end if;
   end loop;

   Initialize_Standard_Files;

   FIO.Chain_File (AP (Standard_In));
   FIO.Chain_File (AP (Standard_Out));
   FIO.Chain_File (AP (Standard_Err));

end Ada.Wide_Wide_Text_IO;
