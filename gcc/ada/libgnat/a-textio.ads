------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. These preconditions
--  are partial and protect against Status_Error, Mode_Error, and Layout_Error,
--  but not against other types of errors.

pragma Assertion_Policy (Pre => Ignore);

--  Note: the generic subpackages of Text_IO (Integer_IO, Float_IO, Fixed_IO,
--  Modular_IO, Decimal_IO and Enumeration_IO) appear as private children in
--  GNAT. These children are with'ed automatically if they are referenced, so
--  this rearrangement is invisible to user programs, but has the advantage
--  that only the needed parts of Text_IO are processed and loaded.

with Ada.IO_Exceptions;
with Ada.Streams;

with System;
with System.File_Control_Block;
with System.WCh_Con;

package Ada.Text_IO with
  Abstract_State    => (File_System),
  Initializes       => (File_System),
  Initial_Condition => Line_Length = 0 and Page_Length = 0
is
   pragma Elaborate_Body;

   type File_Type is limited private with
     Default_Initial_Condition => (not Is_Open (File_Type));
   type File_Mode is (In_File, Out_File, Append_File);

   --  The following representation clause allows the use of unchecked
   --  conversion for rapid translation between the File_Mode type
   --  used in this package and System.File_IO.

   for File_Mode use
     (In_File     => 0,  -- System.FIle_IO.File_Mode'Pos (In_File)
      Out_File    => 2,  -- System.File_IO.File_Mode'Pos (Out_File)
      Append_File => 3); -- System.File_IO.File_Mode'Pos (Append_File)

   type Count is range 0 .. Natural'Last;
   --  The value of Count'Last must be large enough so that the assumption that
   --  the Line, Column and Page counts can never exceed this value is valid.

   subtype Positive_Count is Count range 1 .. Count'Last;

   Unbounded : constant Count := 0;
   --  Line and page length

   subtype Field is Integer range 0 .. 255;
   --  Note: if for any reason, there is a need to increase this value, then it
   --  will be necessary to change the corresponding value in System.Img_Real
   --  in file s-imgrea.adb.

   subtype Number_Base is Integer range 2 .. 16;

   type Type_Set is (Lower_Case, Upper_Case);

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   with
     Pre    => not Is_Open (File),
     Post   =>
       Is_Open (File)
       and then Ada.Text_IO.Mode (File) = Mode
       and then (if Mode /= In_File
                   then (Line_Length (File) = 0
                         and then Page_Length (File) = 0)),
     Global => (In_Out => File_System);

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   with
     Pre    => not Is_Open (File),
     Post   =>
      Is_Open (File)
      and then Ada.Text_IO.Mode (File) = Mode
      and then (if Mode /= In_File
                  then (Line_Length (File) = 0
                        and then Page_Length (File) = 0)),
     Global => (In_Out => File_System);

   procedure Close  (File : in out File_Type) with
     Pre    => Is_Open (File),
     Post   => not Is_Open (File),
     Global => (In_Out => File_System);
   procedure Delete (File : in out File_Type) with
     Pre    => Is_Open (File),
     Post   => not Is_Open (File),
     Global => (In_Out => File_System);
   procedure Reset  (File : in out File_Type; Mode : File_Mode) with
     Pre    => Is_Open (File),
     Post   =>
       Is_Open (File)
       and then Ada.Text_IO.Mode (File) = Mode
       and then (if Mode /= In_File
                   then (Line_Length (File) = 0
                         and then Page_Length (File) = 0)),
     Global => (In_Out => File_System);
   procedure Reset  (File : in out File_Type) with
     Pre    => Is_Open (File),
     Post   =>
       Is_Open (File)
       and Mode (File)'Old = Mode (File)
       and (if Mode (File) /= In_File
                then (Line_Length (File) = 0
                      and then Page_Length (File) = 0)),
     Global => (In_Out => File_System);

   function Mode (File : File_Type) return File_Mode with
     Pre    => Is_Open (File),
     Global => null;
   function Name (File : File_Type) return String with
     Pre    => Is_Open (File),
     Global => null;
   function Form (File : File_Type) return String with
     Pre    => Is_Open (File),
     Global => null;

   function Is_Open (File : File_Type) return Boolean with
     Global => null;

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : File_Type) with SPARK_Mode => Off;
   procedure Set_Output (File : File_Type) with SPARK_Mode => Off;
   procedure Set_Error  (File : File_Type) with SPARK_Mode => Off;

   function Standard_Input  return File_Type with SPARK_Mode => Off;
   function Standard_Output return File_Type with SPARK_Mode => Off;
   function Standard_Error  return File_Type with SPARK_Mode => Off;

   function Current_Input  return File_Type with SPARK_Mode => Off;
   function Current_Output return File_Type with SPARK_Mode => Off;
   function Current_Error  return File_Type with SPARK_Mode => Off;

   type File_Access is access constant File_Type;

   function Standard_Input  return File_Access with SPARK_Mode => Off;
   function Standard_Output return File_Access with SPARK_Mode => Off;
   function Standard_Error  return File_Access with SPARK_Mode => Off;

   function Current_Input  return File_Access with SPARK_Mode => Off;
   function Current_Output return File_Access with SPARK_Mode => Off;
   function Current_Error  return File_Access with SPARK_Mode => Off;

   --------------------
   -- Buffer control --
   --------------------

   --  Note: The parameter file is IN OUT in the RM, but this is clearly
   --  an oversight, and was intended to be IN, see AI95-00057.

   procedure Flush (File : File_Type) with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File)'Old = Line_Length (File)
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);
   procedure Flush with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (File : File_Type; To : Count) with
     Pre    => Is_Open (File)  and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File) = To
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);
   procedure Set_Line_Length (To : Count) with
     Post   =>
       Line_Length = To
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Set_Page_Length (File : File_Type; To : Count) with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Page_Length (File) = To
       and Line_Length (File)'Old = Line_Length (File),
     Global => (In_Out => File_System);
   procedure Set_Page_Length (To : Count) with
     Post   =>
       Page_Length = To
       and Line_Length'Old = Line_Length,
     Global => (In_Out => File_System);

   function Line_Length (File : File_Type) return Count with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Global => (Input => File_System);
   function Line_Length return Count with
     Global => (Input => File_System);

   function Page_Length (File : File_Type) return Count with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Global => (Input => File_System);
   function Page_Length return Count with
     Global => (Input => File_System);

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File)'Old = Line_Length (File)
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);
   procedure New_Line (Spacing : Positive_Count := 1) with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (In_Out => File_System);
   procedure Skip_Line (Spacing : Positive_Count := 1) with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   function End_Of_Line (File : File_Type) return Boolean with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (Input => File_System);
   function End_Of_Line return Boolean with
     Global => (Input => File_System);

   procedure New_Page (File : File_Type) with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File)'Old = Line_Length (File)
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);
   procedure New_Page with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Skip_Page (File : File_Type) with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (In_Out => File_System);
   procedure Skip_Page with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   function End_Of_Page (File : File_Type) return Boolean with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (Input => File_System);
   function End_Of_Page return Boolean with
     Global => (Input => File_System);

   function End_Of_File (File : File_Type) return Boolean with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (Input => File_System);
   function End_Of_File return Boolean with
     Global => (Input => File_System);

   procedure Set_Col (File : File_Type;  To : Positive_Count) with
     Pre            =>
       Is_Open (File)
       and then (if Mode (File) /= In_File
                     then (Line_Length (File) = 0
                           or else To <= Line_Length (File))),
     Contract_Cases =>
       (Mode (File) /= In_File =>
              Line_Length (File)'Old = Line_Length (File)
              and Page_Length (File)'Old = Page_Length (File),
        others                 => True),
     Global         => (In_Out => File_System);
   procedure Set_Col (To : Positive_Count) with
     Pre    => Line_Length = 0 or To <= Line_Length,
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Set_Line (File : File_Type; To : Positive_Count) with
     Pre            =>
       Is_Open (File)
       and then (if Mode (File) /= In_File
                     then (Page_Length (File) = 0
                           or else To <= Page_Length (File))),
     Contract_Cases =>
       (Mode (File) /= In_File =>
              Line_Length (File)'Old = Line_Length (File)
              and Page_Length (File)'Old = Page_Length (File),
        others                 => True),
     Global         => (In_Out => File_System);
   procedure Set_Line (To : Positive_Count) with
     Pre    => Page_Length = 0 or To <= Page_Length,
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   function Col (File : File_Type) return Positive_Count with
     Pre    => Is_Open (File),
     Global => (Input => File_System);
   function Col return Positive_Count with
     Global => (Input => File_System);

   function Line (File : File_Type) return Positive_Count with
     Pre    => Is_Open (File),
     Global => (Input => File_System);
   function Line return Positive_Count with
     Global => (Input => File_System);

   function Page (File : File_Type) return Positive_Count with
     Pre => Is_Open (File),
     Global => (Input => File_System);
   function Page return Positive_Count with
     Global => (Input => File_System);

   ----------------------------
   -- Character Input-Output --
   ----------------------------

   procedure Get (File : File_Type; Item : out Character) with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (In_Out => File_System);
   procedure Get (Item : out Character) with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);
   procedure Put (File : File_Type; Item : Character) with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File)'Old = Line_Length (File)
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);
   procedure Put (Item : Character) with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Character;
      End_Of_Line : out Boolean)
   with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (Input => File_System);

   procedure Look_Ahead
     (Item        : out Character;
      End_Of_Line : out Boolean)
   with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (Input => File_System);

   procedure Get_Immediate
     (File : File_Type;
      Item : out Character)
   with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (In_Out => File_System);

   procedure Get_Immediate
     (Item : out Character)
   with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Character;
      Available : out Boolean)
   with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (In_Out => File_System);

   procedure Get_Immediate
     (Item      : out Character;
      Available : out Boolean)
   with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   -------------------------
   -- String Input-Output --
   -------------------------

   procedure Get (File : File_Type; Item : out String) with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Global => (In_Out => File_System);
   procedure Get (Item : out String) with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);
   procedure Put (File : File_Type; Item : String) with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File)'Old = Line_Length (File)
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);
   procedure Put (Item : String) with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   procedure Get_Line
     (File : File_Type;
      Item : out String;
      Last : out Natural)
   with
     Pre    => Is_Open (File) and then Mode (File) = In_File,
     Post   => (if Item'Length > 0 then Last in Item'First - 1 .. Item'Last
               else Last = Item'First - 1),
     Global => (In_Out => File_System);

   procedure Get_Line
     (Item : out String;
      Last : out Natural)
   with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length
       and (if Item'Length > 0 then Last in Item'First - 1 .. Item'Last
            else Last = Item'First - 1),
     Global => (In_Out => File_System);

   function Get_Line (File : File_Type) return String with SPARK_Mode => Off;
   pragma Ada_05 (Get_Line);

   function Get_Line return String with SPARK_Mode => Off;
   pragma Ada_05 (Get_Line);

   procedure Put_Line
     (File : File_Type;
      Item : String)
   with
     Pre    => Is_Open (File) and then Mode (File) /= In_File,
     Post   =>
       Line_Length (File)'Old = Line_Length (File)
       and Page_Length (File)'Old = Page_Length (File),
     Global => (In_Out => File_System);

   procedure Put_Line
     (Item : String)
   with
     Post   =>
       Line_Length'Old = Line_Length
       and Page_Length'Old = Page_Length,
     Global => (In_Out => File_System);

   ---------------------------------------
   -- Generic packages for Input-Output --
   ---------------------------------------

   --  The generic packages:

   --    Ada.Text_IO.Integer_IO
   --    Ada.Text_IO.Modular_IO
   --    Ada.Text_IO.Float_IO
   --    Ada.Text_IO.Fixed_IO
   --    Ada.Text_IO.Decimal_IO
   --    Ada.Text_IO.Enumeration_IO

   --  are implemented as separate child packages in GNAT, so the
   --  spec and body of these packages are to be found in separate
   --  child units. This implementation detail is hidden from the
   --  Ada programmer by special circuitry in the compiler that
   --  treats these child packages as though they were nested in
   --  Text_IO. The advantage of this special processing is that
   --  the subsidiary routines needed if these generics are used
   --  are not loaded when they are not used.

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;
   Layout_Error : exception renames IO_Exceptions.Layout_Error;

private

   --  The following procedures have a File_Type formal of mode IN OUT because
   --  they may close the original file. The Close operation may raise an
   --  exception, but in that case we want any assignment to the formal to
   --  be effective anyway, so it must be passed by reference (or the caller
   --  will be left with a dangling pointer).

   pragma Export_Procedure
     (Internal  => Close,
      External  => "",
      Mechanism => Reference);
   pragma Export_Procedure
     (Internal  => Delete,
      External  => "",
      Mechanism => Reference);
   pragma Export_Procedure
     (Internal        => Reset,
      External        => "",
      Parameter_Types => (File_Type),
      Mechanism       => Reference);
   pragma Export_Procedure
     (Internal        => Reset,
      External        => "",
      Parameter_Types => (File_Type, File_Mode),
      Mechanism       => (File => Reference));

   -----------------------------------
   -- Handling of Format Characters --
   -----------------------------------

   --  Line marks are represented by the single character ASCII.LF (16#0A#).
   --  In DOS and similar systems, underlying file translation takes care
   --  of translating this to and from the standard CR/LF sequences used in
   --  these operating systems to mark the end of a line. On output there is
   --  always a line mark at the end of the last line, but on input, this
   --  line mark can be omitted, and is implied by the end of file.

   --  Page marks are represented by the single character ASCII.FF (16#0C#),
   --  The page mark at the end of the file may be omitted, and is normally
   --  omitted on output unless an explicit New_Page call is made before
   --  closing the file. No page mark is added when a file is appended to,
   --  so, in accordance with the permission in (RM A.10.2(4)), there may
   --  or may not be a page mark separating preexisting text in the file
   --  from the new text to be written.

   --  A file mark is marked by the physical end of file. In DOS translation
   --  mode on input, an EOF character (SUB = 16#1A#) gets translated to the
   --  physical end of file, so in effect this character is recognized as
   --  marking the end of file in DOS and similar systems.

   LM : constant := Character'Pos (ASCII.LF);
   --  Used as line mark

   PM : constant := Character'Pos (ASCII.FF);
   --  Used as page mark, except at end of file where it is implied

   --------------------------------
   -- Text_IO File Control Block --
   --------------------------------

   Default_WCEM : System.WCh_Con.WC_Encoding_Method :=
                    System.WCh_Con.WCEM_UTF8;
   --  This gets modified during initialization (see body) using
   --  the default value established in the call to Set_Globals.

   package FCB renames System.File_Control_Block;

   type Text_AFCB;
   type File_Type is access all Text_AFCB;

   type Text_AFCB is new FCB.AFCB with record
      Page        : Count := 1;
      Line        : Count := 1;
      Col         : Count := 1;
      Line_Length : Count := 0;
      Page_Length : Count := 0;

      Self : aliased File_Type;
      --  Set to point to the containing Text_AFCB block. This is used to
      --  implement the Current_{Error,Input,Output} functions which return
      --  a File_Access, the file access value returned is a pointer to
      --  the Self field of the corresponding file.

      Before_LM : Boolean := False;
      --  This flag is used to deal with the anomalies introduced by the
      --  peculiar definition of End_Of_File and End_Of_Page in Ada. These
      --  functions require looking ahead more than one character. Since
      --  there is no convenient way of backing up more than one character,
      --  what we do is to leave ourselves positioned past the LM, but set
      --  this flag, so that we know that from an Ada point of view we are
      --  in front of the LM, not after it. A little odd, but it works.

      Before_LM_PM : Boolean := False;
      --  This flag similarly handles the case of being physically positioned
      --  after a LM-PM sequence when logically we are before the LM-PM. This
      --  flag can only be set if Before_LM is also set.

      WC_Method : System.WCh_Con.WC_Encoding_Method := Default_WCEM;
      --  Encoding method to be used for this file. Text_IO does not deal with
      --  wide characters, but it does deal with upper half characters in the
      --  range 16#80#-16#FF# which may need encoding, e.g. in UTF-8 mode.

      Before_Upper_Half_Character : Boolean := False;
      --  This flag is set to indicate that an encoded upper half character has
      --  been read by Text_IO.Look_Ahead. If it is set to True, then it means
      --  that the stream is logically positioned before the character but is
      --  physically positioned after it. The character involved must be in
      --  the range 16#80#-16#FF#, i.e. if the flag is set, then we know the
      --  next character has a code greater than 16#7F#, and the value of this
      --  character is saved in Saved_Upper_Half_Character.

      Saved_Upper_Half_Character : Character;
      --  This field is valid only if Before_Upper_Half_Character is set. It
      --  contains an upper-half character read by Look_Ahead. If Look_Ahead
      --  reads a character in the range 16#00# to 16#7F#, then it can use
      --  ungetc to put it back, but ungetc cannot be called more than once,
      --  so for characters above this range, we don't try to back up the
      --  file. Instead we save the character in this field and set the flag
      --  Before_Upper_Half_Character to True to indicate that we are logically
      --  positioned before this character even though the stream is physically
      --  positioned after it.

   end record;

   function AFCB_Allocate (Control_Block : Text_AFCB) return FCB.AFCB_Ptr;

   procedure AFCB_Close (File : not null access Text_AFCB);
   procedure AFCB_Free  (File : not null access Text_AFCB);

   procedure Read
     (File : in out Text_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Read operation used when Text_IO file is treated directly as Stream

   procedure Write
     (File : in out Text_AFCB;
      Item : Ada.Streams.Stream_Element_Array);
   --  Write operation used when Text_IO file is treated directly as Stream

   ------------------------
   -- The Standard Files --
   ------------------------

   Standard_In_AFCB  : aliased Text_AFCB;
   Standard_Out_AFCB : aliased Text_AFCB;
   Standard_Err_AFCB : aliased Text_AFCB;

   Standard_In  : aliased File_Type := Standard_In_AFCB'Access with
     Part_Of => File_System;
   Standard_Out : aliased File_Type := Standard_Out_AFCB'Access with
     Part_Of => File_System;
   Standard_Err : aliased File_Type := Standard_Err_AFCB'Access with
     Part_Of => File_System;
   --  Standard files

   Current_In   : aliased File_Type := Standard_In with
     Part_Of => File_System;
   Current_Out  : aliased File_Type := Standard_Out with
     Part_Of => File_System;
   Current_Err  : aliased File_Type := Standard_Err with
     Part_Of => File_System;
   --  Current files

   function EOF_Char return Integer;
   --  Returns the system-specific character indicating the end of a text file.
   --  This is exported for use by child packages such as Enumeration_Aux to
   --  eliminate their needing to depend directly on Interfaces.C_Streams,
   --  which is not available in certain target environments (such as AAMP).

   procedure Initialize_Standard_Files;
   --  Initializes the file control blocks for the standard files. Called from
   --  the elaboration routine for this package, and from Reset_Standard_Files
   --  in package Ada.Text_IO.Reset_Standard_Files.

end Ada.Text_IO;
