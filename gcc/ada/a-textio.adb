------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Ada.Streams;          use Ada.Streams;
with Interfaces.C_Streams; use Interfaces.C_Streams;
with System;
with System.File_IO;
with System.CRTL;
with Unchecked_Conversion;
with Unchecked_Deallocation;

pragma Elaborate_All (System.File_IO);
--  Needed because of calls to Chain_File in package body elaboration

package body Ada.Text_IO is

   package FIO renames System.File_IO;

   subtype AP is FCB.AFCB_Ptr;

   function To_FCB is new Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_TIO is new Unchecked_Conversion (FCB.File_Mode, File_Mode);
   use type FCB.File_Mode;

   use type System.CRTL.size_t;

   -------------------
   -- AFCB_Allocate --
   -------------------

   function AFCB_Allocate (Control_Block : Text_AFCB) return FCB.AFCB_Ptr is
      pragma Unreferenced (Control_Block);
   begin
      return new Text_AFCB;
   end AFCB_Allocate;

   ----------------
   -- AFCB_Close --
   ----------------

   procedure AFCB_Close (File : access Text_AFCB) is
   begin
      --  If the file being closed is one of the current files, then close
      --  the corresponding current file. It is not clear that this action
      --  is required (RM A.10.3(23)) but it seems reasonable, and besides
      --  ACVC test CE3208A expects this behavior.

      if File_Type (File) = Current_In then
         Current_In := null;
      elsif File_Type (File) = Current_Out then
         Current_Out := null;
      elsif File_Type (File) = Current_Err then
         Current_Err := null;
      end if;

      Terminate_Line (File_Type (File));
   end AFCB_Close;

   ---------------
   -- AFCB_Free --
   ---------------

   procedure AFCB_Free (File : access Text_AFCB) is
      type FCB_Ptr is access all Text_AFCB;
      FT : FCB_Ptr := FCB_Ptr (File);

      procedure Free is new Unchecked_Deallocation (Text_AFCB, FCB_Ptr);

   begin
      Free (FT);
   end AFCB_Free;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File));
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
      Dummy_File_Control_Block : Text_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'T',
                Creat     => True,
                Text      => True);

      File.Self := File;
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
      FIO.Delete (AP (File));
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_LM then

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

      if File.Before_LM then
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
      ch  : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if not File.Is_Regular_File then
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

   --------------
   -- EOF_Char --
   --------------

   function EOF_Char return Integer is
   begin
      return EOF;
   end EOF_Char;

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
      Item : out Character)
   is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_LM then
         File.Before_LM := False;
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
   end Get;

   procedure Get (Item : out Character) is
   begin
      Get (Current_In, Item);
   end Get;

   procedure Get
     (File : File_Type;
      Item : out String)
   is
      ch : int;
      J  : Natural;

   begin
      FIO.Check_Read_Status (AP (File));

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

      J := Item'First;
      while J <= Item'Last loop
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
            Item (J) := Character'Val (ch);
            J := J + 1;
            File.Col := File.Col + 1;
         end if;
      end loop;
   end Get;

   procedure Get (Item : out String) is
   begin
      Get (Current_In, Item);
   end Get;

   -------------------
   -- Get_Immediate --
   -------------------

   --  More work required here ???

   procedure Get_Immediate
     (File : File_Type;
      Item : out Character)
   is
      ch          : int;
      end_of_file : int;

      procedure getc_immediate
        (stream      : FILEs;
         ch          : out int;
         end_of_file : out int);
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
            raise End_Error;
         end if;
      end if;

      Item := Character'Val (ch);
   end Get_Immediate;

   procedure Get_Immediate
     (Item : out Character)
   is
   begin
      Get_Immediate (Current_In, Item);
   end Get_Immediate;

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Character;
      Available : out Boolean)
   is
      ch          : int;
      end_of_file : int;
      avail       : int;

      procedure getc_immediate_nowait
        (stream      : FILEs;
         ch          : out int;
         end_of_file : out int;
         avail       : out int);
      pragma Import (C, getc_immediate_nowait, "getc_immediate_nowait");

   begin
      FIO.Check_Read_Status (AP (File));

      --  If we are logically before an end of line, but physically after it,
      --  then we just return the end of line character, no I/O is necessary.

      if File.Before_LM then
         File.Before_LM := False;
         File.Before_LM_PM := False;

         Available := True;
         Item := Character'Val (LM);

      --  Normal case where a read operation is required

      else
         getc_immediate_nowait (File.Stream, ch, end_of_file, avail);

         if ferror (File.Stream) /= 0 then
            raise Device_Error;

         elsif end_of_file /= 0 then
            raise End_Error;

         elsif avail = 0 then
            Available := False;
            Item := ASCII.NUL;

         else
            Available := True;
            Item := Character'Val (ch);
         end if;
      end if;

   end Get_Immediate;

   procedure Get_Immediate
     (Item      : out Character;
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
      Item : out String;
      Last : out Natural)
   is
      ch : int;

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
         ch := Getc (File);

         --  If we are at the end of file now, it means we are trying to
         --  skip a file terminator and we raise End_Error (RM A.10.7(20))

         if ch = EOF then
            raise End_Error;
         end if;

         --  Loop through characters. Don't bother if we hit a page mark,
         --  since in normal files, page marks can only follow line marks
         --  in any case and we only promise to treat the page nonsense
         --  correctly in the absense of such rogue page marks.

         loop
            --  Exit the loop if read is terminated by encountering line mark

            exit when ch = LM;

            --  Otherwise store the character, note that we know that ch is
            --  something other than LM or EOF. It could possibly be a page
            --  mark if there is a stray page mark in the middle of a line,
            --  but this is not an official page mark in any case, since
            --  official page marks can only follow a line mark. The whole
            --  page business is pretty much nonsense anyway, so we do not
            --  want to waste time trying to make sense out of non-standard
            --  page marks in the file! This means that the behavior of
            --  Get_Line is different from repeated Get of a character, but
            --  that's too bad. We only promise that page numbers etc make
            --  sense if the file is formatted in a standard manner.

            --  Note: we do not adjust the column number because it is quicker
            --  to adjust it once at the end of the operation than incrementing
            --  it each time around the loop.

            Last := Last + 1;
            Item (Last) := Character'Val (ch);

            --  All done if the string is full, this is the case in which
            --  we do not skip the following line mark. We need to adjust
            --  the column number in this case.

            if Last = Item'Last then
               File.Col := File.Col + Count (Item'Length);
               return;
            end if;

            --  Otherwise read next character. We also exit from the loop if
            --  we read an end of file. This is the case where the last line
            --  is not terminated with a line mark, and we consider that there
            --  is an implied line mark in this case (this is a non-standard
            --  file, but it is nice to treat it reasonably).

            ch := Getc (File);
            exit when ch = EOF;
         end loop;
      end if;

      --  We have skipped past, but not stored, a line mark. Skip following
      --  page mark if one follows, but do not do this for a non-regular
      --  file (since otherwise we get annoying wait for an extra character)

      File.Line := File.Line + 1;
      File.Col := 1;

      if File.Before_LM_PM then
         File.Line := 1;
         File.Before_LM_PM := False;
         File.Page := File.Page + 1;

      elsif File.Is_Regular_File then
         ch := Getc (File);

         if ch = PM and then File.Is_Regular_File then
            File.Line := 1;
            File.Page := File.Page + 1;
         else
            Ungetc (ch, File);
         end if;
      end if;
   end Get_Line;

   procedure Get_Line
     (Item : out String;
      Last : out Natural)
   is
   begin
      Get_Line (Current_In, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return String is
      Buffer : String (1 .. 500);
      Last   : Natural;

      function Get_Rest (S : String) return String;
      --  This is a recursive function that reads the rest of the line and
      --  returns it. S is the part read so far.

      --------------
      -- Get_Rest --
      --------------

      function Get_Rest (S : String) return String is

         --  Each time we allocate a buffer the same size as what we have
         --  read so far. This limits us to a logarithmic number of calls
         --  to Get_Rest and also ensures only a linear use of stack space.

         Buffer : String (1 .. S'Length);
         Last   : Natural;

      begin
         Get_Line (File, Buffer, Last);

         declare
            R : constant String := S & Buffer (1 .. Last);
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

   function Get_Line return String is
   begin
      return Get_Line (Current_In);
   end Get_Line;

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
      Item        : out Character;
      End_Of_Line : out Boolean)
   is
      ch : int;

   begin
      FIO.Check_Read_Status (AP (File));

      if File.Before_LM then
         End_Of_Line := True;
         Item := ASCII.NUL;

      else
         ch := Nextc (File);

         if ch = LM
           or else ch = EOF
           or else (ch = PM and then File.Is_Regular_File)
         then
            End_Of_Line := True;
            Item := ASCII.NUL;
         else
            End_Of_Line := False;
            Item := Character'Val (ch);
         end if;
      end if;
   end Look_Ahead;

   procedure Look_Ahead
     (Item        : out Character;
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

      if Spacing not in Positive_Count then
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
      Dummy_File_Control_Block : Text_AFCB;
      pragma Warnings (Off, Dummy_File_Control_Block);
      --  Yes, we know this is never assigned a value, only the tag
      --  is used for dispatching purposes, so that's expected.

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => Dummy_File_Control_Block,
                Mode      => To_FCB (Mode),
                Name      => Name,
                Form      => Form,
                Amethod   => 'T',
                Creat     => False,
                Text      => True);

      File.Self := File;
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
      Item : Character)
   is
   begin
      FIO.Check_Write_Status (AP (File));

      if File.Line_Length /= 0 and then File.Col > File.Line_Length then
         New_Line (File);
      end if;

      if fputc (Character'Pos (Item), File.Stream) = EOF then
         raise Device_Error;
      end if;

      File.Col := File.Col + 1;
   end Put;

   procedure Put (Item : Character) is
   begin
      FIO.Check_Write_Status (AP (Current_Out));

      if Current_Out.Line_Length /= 0
        and then Current_Out.Col > Current_Out.Line_Length
      then
         New_Line (Current_Out);
      end if;

      if fputc (Character'Pos (Item), Current_Out.Stream) = EOF then
         raise Device_Error;
      end if;

      Current_Out.Col := Current_Out.Col + 1;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : String)
   is
   begin
      FIO.Check_Write_Status (AP (File));

      if Item'Length > 0 then

         --  If we have bounded lines, then do things character by
         --  character (this seems a rare case anyway!)

         if File.Line_Length /= 0 then
            for J in Item'Range loop
               Put (File, Item (J));
            end loop;

         --  Otherwise we can output the entire string at once. Note that if
         --  there are LF or FF characters in the string, we do not bother to
         --  count them as line or page terminators.

         else
            FIO.Write_Buf (AP (File), Item'Address, Item'Length);
            File.Col := File.Col + Item'Length;
         end if;
      end if;
   end Put;

   procedure Put (Item : String) is
   begin
      Put (Current_Out, Item);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File : File_Type;
      Item : String)
   is
      Ilen   : Natural := Item'Length;
      Istart : Natural := Item'First;

   begin
      FIO.Check_Write_Status (AP (File));

      --  If we have bounded lines, then just do a put and a new line. In
      --  this case we will end up doing things character by character in
      --  any case, and it is a rare situation.

      if File.Line_Length /= 0 then
         Put (File, Item);
         New_Line (File);
         return;
      end if;

      --  We setup a single string that has the necessary terminators and
      --  then write it with a single call. The reason for doing this is
      --  that it gives better behavior for the use of Put_Line in multi-
      --  tasking programs, since often the OS will treat the entire put
      --  operation as an atomic operation.

      --  We only do this if the message is 512 characters or less in length,
      --  since otherwise Put_Line would use an unbounded amount of stack
      --  space and could cause undetected stack overflow. If we have a
      --  longer string, then output the first part separately to avoid this.

      if Ilen > 512 then
         FIO.Write_Buf (AP (File), Item'Address, size_t (Ilen - 512));
         Istart := Istart + Ilen - 512;
         Ilen   := 512;
      end if;

      --  Now prepare the string with its terminator

      declare
         Buffer : String (1 .. Ilen + 2);
         Plen   : size_t;

      begin
         Buffer (1 .. Ilen) := Item (Istart .. Item'Last);
         Buffer (Ilen + 1) := Character'Val (LM);

         if File.Page_Length /= 0
           and then File.Line > File.Page_Length
         then
            Buffer (Ilen + 2) := Character'Val (PM);
            Plen := size_t (Ilen) + 2;
            File.Line := 1;
            File.Page := File.Page + 1;

         else
            Plen := size_t (Ilen) + 1;
            File.Line := File.Line + 1;
         end if;

         FIO.Write_Buf (AP (File), Buffer'Address, Plen);

         File.Col := 1;
      end;
   end Put_Line;

   procedure Put_Line (Item : String) is
   begin
      Put_Line (Current_Out, Item);
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
     (File : in out Text_AFCB;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Discard_ch : int;
      pragma Warnings (Off, Discard_ch);

   begin
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
      FIO.Reset (AP (File), To_FCB (Mode));
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
      FIO.Reset (AP (File));
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

      if To not in Positive_Count then
         raise Constraint_Error;
      end if;

      FIO.Check_File_Open (AP (File));

      --  Output case

      if Mode (File) >= Out_File then

         --  Error if we attempt to set Col to a value greater than the
         --  maximum permissible line length.

         if File.Line_Length /= 0 and then To > File.Line_Length then
            raise Layout_Error;
         end if;

         --  If we are behind current position, then go to start of new line

         if To < File.Col then
            New_Line (File);
         end if;

         --  Loop to output blanks till we are at the required column

         while File.Col < To loop
            Put (File, ' ');
         end loop;

      --  Input case

      else
         --  If we are logically before a LM, but physically after it, the
         --  file position still reflects the position before the LM, so eat
         --  it now and adjust the file position appropriately.

         if File.Before_LM then
            File.Before_LM := False;
            File.Before_LM_PM := False;
            File.Line := File.Line + 1;
            File.Col := 1;
         end if;

         --  Loop reading characters till we get one at the required Col value

         loop
            --  Read next character. The reason we have to read ahead is to
            --  skip formatting characters, the effect of Set_Col is to set
            --  us to a real character with the right Col value, and format
            --  characters don't count.

            ch := Getc (File);

            --  Error if we hit an end of file

            if ch = EOF then
               raise End_Error;

            --  If line mark, eat it and adjust file position

            elsif ch = LM then
               File.Line := File.Line + 1;
               File.Col := 1;

            --  If recognized page mark, eat it, and adjust file position

            elsif ch = PM and then File.Is_Regular_File then
               File.Page := File.Page + 1;
               File.Line := 1;
               File.Col := 1;

            --  Otherwise this is the character we are looking for, so put it
            --  back in the input stream (we have not adjusted the file
            --  position yet, so everything is set right after this ungetc).

            elsif To = File.Col then
               Ungetc (ch, File);
               return;

            --  Keep skipping characters if we are not there yet, updating the
            --  file position past the skipped character.

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

      if To not in Positive_Count then
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

      if To not in Count then
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

      if To not in Count then
         raise Constraint_Error;
      end if;

      FIO.Check_Write_Status (AP (File));
      File.Page_Length := To;
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Current_Out, To);
   end Set_Page_Length;

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

      if Spacing not in Positive_Count then
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
               exit when ch = LM or ch = EOF;
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

         --  We also avoid this for files opened in append mode, in
         --  accordance with (RM A.8.2(10))

         elsif (File /= Standard_Err and then File /= Standard_Out)
           and then (File.Line = 1 and then File.Page = 1)
           and then Mode (File) = Out_File
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
     (File : in out Text_AFCB;
      Item : Stream_Element_Array)
   is

      function Has_Translated_Characters return Boolean;
      --  return True if Item array contains a character which will be
      --  translated under the text file mode. There is only one such
      --  character under DOS based systems which is character 10.

      text_translation_required : Boolean;
      pragma Import (C, text_translation_required,
                     "__gnat_text_translation_required");

      Siz : constant size_t := Item'Length;

      function Has_Translated_Characters return Boolean is
      begin
         for K in Item'Range loop
            if Item (K) = 10 then
               return True;
            end if;
         end loop;
         return False;
      end Has_Translated_Characters;

      Needs_Binary_Write : constant Boolean :=
        text_translation_required and then Has_Translated_Characters;

   begin
      if File.Mode = FCB.In_File then
         raise Mode_Error;
      end if;

      --  Now we do the write. Since this is a text file, it is normally in
      --  text mode, but stream data must be written in binary mode, so we
      --  temporarily set binary mode for the write, resetting it after. This
      --  is done only if needed (i.e. there is some characters in Item which
      --  needs to be written using the binary mode).
      --  These calls have no effect in a system (like Unix) where there is
      --  no distinction between text and binary files.

      --  Since the character translation is done at the time the buffer is
      --  written (this is true under Windows) we first flush current buffer
      --  with text mode if needed.

      if Needs_Binary_Write then

         if fflush (File.Stream) = -1 then
            raise Device_Error;
         end if;

         set_binary_mode (fileno (File.Stream));
      end if;

      if fwrite (Item'Address, 1, Siz, File.Stream) /= Siz then
         raise Device_Error;
      end if;

      --  At this point we need to flush the buffer using the binary mode then
      --  we reset to text mode.

      if Needs_Binary_Write then

         if fflush (File.Stream) = -1 then
            raise Device_Error;
         end if;

         set_text_mode (fileno (File.Stream));
      end if;
   end Write;

   --  Use "preallocated" strings to avoid calling "new" during the
   --  elaboration of the run time. This is needed in the tasking case to
   --  avoid calling Task_Lock too early. A filename is expected to end with a
   --  null character in the runtime, here the null characters are added just
   --  to have a correct filename length.

   Err_Name : aliased String := "*stderr" & ASCII.Nul;
   In_Name  : aliased String := "*stdin" & ASCII.Nul;
   Out_Name : aliased String := "*stdout" & ASCII.Nul;
begin
   -------------------------------
   -- Initialize Standard Files --
   -------------------------------

   --  Note: the names in these files are bogus, and probably it would be
   --  better for these files to have no names, but the ACVC test insist!
   --  We use names that are bound to fail in open etc.

   Standard_Err.Stream            := stderr;
   Standard_Err.Name              := Err_Name'Access;
   Standard_Err.Form              := Null_Str'Unrestricted_Access;
   Standard_Err.Mode              := FCB.Out_File;
   Standard_Err.Is_Regular_File   := is_regular_file (fileno (stderr)) /= 0;
   Standard_Err.Is_Temporary_File := False;
   Standard_Err.Is_System_File    := True;
   Standard_Err.Is_Text_File      := True;
   Standard_Err.Access_Method     := 'T';
   Standard_Err.Self              := Standard_Err;

   Standard_In.Stream             := stdin;
   Standard_In.Name               := In_Name'Access;
   Standard_In.Form               := Null_Str'Unrestricted_Access;
   Standard_In.Mode               := FCB.In_File;
   Standard_In.Is_Regular_File    := is_regular_file (fileno (stdin)) /= 0;
   Standard_In.Is_Temporary_File  := False;
   Standard_In.Is_System_File     := True;
   Standard_In.Is_Text_File       := True;
   Standard_In.Access_Method      := 'T';
   Standard_In.Self               := Standard_In;

   Standard_Out.Stream            := stdout;
   Standard_Out.Name              := Out_Name'Access;
   Standard_Out.Form              := Null_Str'Unrestricted_Access;
   Standard_Out.Mode              := FCB.Out_File;
   Standard_Out.Is_Regular_File   := is_regular_file (fileno (stdout)) /= 0;
   Standard_Out.Is_Temporary_File := False;
   Standard_Out.Is_System_File    := True;
   Standard_Out.Is_Text_File      := True;
   Standard_Out.Access_Method     := 'T';
   Standard_Out.Self              := Standard_Out;

   FIO.Chain_File (AP (Standard_In));
   FIO.Chain_File (AP (Standard_Out));
   FIO.Chain_File (AP (Standard_Err));

   FIO.Make_Unbuffered (AP (Standard_Out));
   FIO.Make_Unbuffered (AP (Standard_Err));

end Ada.Text_IO;
