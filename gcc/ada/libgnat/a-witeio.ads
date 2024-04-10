------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . W I D E _ T E X T _ I O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  Note: the generic subpackages of Wide_Text_IO (Integer_IO, Float_IO,
--  Fixed_IO, Modular_IO, Decimal_IO and Enumeration_IO) appear as private
--  children in GNAT. These children are with'ed automatically if they are
--  referenced, so this rearrangement is invisible to user programs, but has
--  the advantage that only the needed parts of Wide_Text_IO are processed
--  and loaded.

with Ada.IO_Exceptions;
with Ada.Streams;

with Interfaces.C_Streams;

with System;
with System.File_Control_Block;
with System.WCh_Con;

package Ada.Wide_Text_IO is

   type File_Type is limited private with Default_Initial_Condition;
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

   subtype Number_Base is Integer range 2 .. 16;

   type Type_Set is (Lower_Case, Upper_Case);

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "");

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : File_Mode);
   procedure Reset  (File : in out File_Type);

   function Mode (File : File_Type) return File_Mode;
   function Name (File : File_Type) return String;
   function Form (File : File_Type) return String;

   function Is_Open (File : File_Type) return Boolean;

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : File_Type);
   procedure Set_Output (File : File_Type);
   procedure Set_Error  (File : File_Type);

   function Standard_Input  return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error  return File_Type;

   function Current_Input  return File_Type;
   function Current_Output return File_Type;
   function Current_Error  return File_Type;

   type File_Access is access constant File_Type;

   function Standard_Input  return File_Access;
   function Standard_Output return File_Access;
   function Standard_Error  return File_Access;

   function Current_Input  return File_Access;
   function Current_Output return File_Access;
   function Current_Error  return File_Access;

   --------------------
   -- Buffer control --
   --------------------

   --  Note: The parameter file is in out in the RM, but as pointed out
   --  in <<95-5166.a Tucker Taft 95-6-23>> this is clearly an oversight.

   procedure Flush (File : File_Type);
   procedure Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (File : File_Type; To : Count);
   procedure Set_Line_Length (To : Count);

   procedure Set_Page_Length (File : File_Type; To : Count);
   procedure Set_Page_Length (To : Count);

   function Line_Length (File : File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : File_Type) return Count;
   function Page_Length return Count;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1);
   procedure New_Line (Spacing : Positive_Count := 1);

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1);
   procedure Skip_Line (Spacing : Positive_Count := 1);

   function End_Of_Line (File : File_Type) return Boolean;
   function End_Of_Line return Boolean;

   procedure New_Page (File : File_Type);
   procedure New_Page;

   procedure Skip_Page (File : File_Type);
   procedure Skip_Page;

   function End_Of_Page (File : File_Type) return Boolean;
   function End_Of_Page return Boolean;

   function End_Of_File (File : File_Type) return Boolean;
   function End_Of_File return Boolean;

   procedure Set_Col (File : File_Type;  To : Positive_Count);
   procedure Set_Col (To : Positive_Count);

   procedure Set_Line (File : File_Type; To : Positive_Count);
   procedure Set_Line (To : Positive_Count);

   function Col (File : File_Type) return Positive_Count;
   function Col return Positive_Count;

   function Line (File : File_Type) return Positive_Count;
   function Line return Positive_Count;

   function Page (File : File_Type) return Positive_Count;
   function Page return Positive_Count;

   ----------------------------
   -- Character Input-Output --
   ----------------------------

   procedure Get (File : File_Type; Item : out Wide_Character);
   procedure Get (Item : out Wide_Character);
   procedure Put (File : File_Type; Item : Wide_Character);
   procedure Put (Item : Wide_Character);

   procedure Look_Ahead
     (File        : File_Type;
      Item        : out Wide_Character;
      End_Of_Line : out Boolean);

   procedure Look_Ahead
     (Item        : out Wide_Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate
     (File : File_Type;
      Item : out Wide_Character);

   procedure Get_Immediate
     (Item : out Wide_Character);

   procedure Get_Immediate
     (File      : File_Type;
      Item      : out Wide_Character;
      Available : out Boolean);

   procedure Get_Immediate
     (Item      : out Wide_Character;
      Available : out Boolean);

   -------------------------
   -- String Input-Output --
   -------------------------

   procedure Get (File : File_Type; Item : out Wide_String);
   procedure Get (Item : out Wide_String);
   procedure Put (File : File_Type; Item : Wide_String);
   procedure Put (Item : Wide_String);

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_String;
      Last : out Natural);

   procedure Get_Line
     (Item : out Wide_String;
      Last : out Natural);

   function Get_Line (File : File_Type) return Wide_String;
   pragma Ada_05 (Get_Line);

   function Get_Line return Wide_String;
   pragma Ada_05 (Get_Line);

   procedure Put_Line
     (File : File_Type;
      Item : Wide_String);

   procedure Put_Line
     (Item : Wide_String);

   ---------------------------------------
   -- Generic packages for Input-Output --
   ---------------------------------------

   --  The generic packages:

   --    Ada.Wide_Text_IO.Integer_IO
   --    Ada.Wide_Text_IO.Modular_IO
   --    Ada.Wide_Text_IO.Float_IO
   --    Ada.Wide_Text_IO.Fixed_IO
   --    Ada.Wide_Text_IO.Decimal_IO
   --    Ada.Wide_Text_IO.Enumeration_IO

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

   package WCh_Con renames System.WCh_Con;

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

   -------------------------------------
   -- Wide_Text_IO File Control Block --
   -------------------------------------

   Default_WCEM : WCh_Con.WC_Encoding_Method := WCh_Con.WCEM_UTF8;
   --  This gets modified during initialization (see body) using
   --  the default value established in the call to Set_Globals.

   package FCB renames System.File_Control_Block;

   type Wide_Text_AFCB is new FCB.AFCB with record
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
      --  in front of the LM, not after it. A bit odd, but it works.

      Before_LM_PM : Boolean := False;
      --  This flag similarly handles the case of being physically positioned
      --  after a LM-PM sequence when logically we are before the LM-PM. This
      --  flag can only be set if Before_LM is also set.

      WC_Method : WCh_Con.WC_Encoding_Method := Default_WCEM;
      --  Encoding method to be used for this file

      Before_Wide_Character : Boolean := False;
      --  This flag is set to indicate that a wide character in the input has
      --  been read by Wide_Text_IO.Look_Ahead. If it is set to True, then it
      --  means that the stream is logically positioned before the character
      --  but is physically positioned after it. The character involved must
      --  not be in the range 16#00#-16#7F#, i.e. if the flag is set, then
      --  we know the next character has a code greater than 16#7F#, and the
      --  value of this character is saved in Saved_Wide_Character.

      Saved_Wide_Character : Wide_Character;
      --  This field is valid only if Before_Wide_Character is set. It
      --  contains a wide character read by Look_Ahead. If Look_Ahead
      --  reads a character in the range 16#0000# to 16#007F#, then it
      --  can use ungetc to put it back, but ungetc cannot be called
      --  more than once, so for characters above this range, we don't
      --  try to back up the file. Instead we save the character in this
      --  field and set the flag Before_Wide_Character to indicate that
      --  we are logically positioned before this character even though
      --  the stream is physically positioned after it.

   end record;

   type File_Type is access all Wide_Text_AFCB;

   function AFCB_Allocate (Control_Block : Wide_Text_AFCB) return FCB.AFCB_Ptr;

   procedure AFCB_Close (File : not null access Wide_Text_AFCB);
   procedure AFCB_Free  (File : not null access Wide_Text_AFCB);

   procedure Read
     (File : in out Wide_Text_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Read operation used when Wide_Text_IO file is treated as a Stream

   procedure Write
     (File : in out Wide_Text_AFCB;
      Item : Ada.Streams.Stream_Element_Array);
   --  Write operation used when Wide_Text_IO file is treated as a Stream

   ------------------------
   -- The Standard Files --
   ------------------------

   Standard_Err_AFCB : aliased Wide_Text_AFCB;
   Standard_In_AFCB  : aliased Wide_Text_AFCB;
   Standard_Out_AFCB : aliased Wide_Text_AFCB;

   Standard_Err : aliased File_Type := Standard_Err_AFCB'Access;
   Standard_In  : aliased File_Type := Standard_In_AFCB'Access;
   Standard_Out : aliased File_Type := Standard_Out_AFCB'Access;
   --  Standard files

   Current_In   : aliased File_Type := Standard_In;
   Current_Out  : aliased File_Type := Standard_Out;
   Current_Err  : aliased File_Type := Standard_Err;
   --  Current files

   procedure Initialize_Standard_Files;
   --  Initializes the file control blocks for the standard files. Called from
   --  the elaboration routine for this package, and from Reset_Standard_Files
   --  in package Ada.Wide_Text_IO.Reset_Standard_Files.

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  These subprograms are in the private part of the spec so that they can
   --  be shared by the children of Ada.Wide_Text_IO.

   function Getc (File : File_Type) return Interfaces.C_Streams.int;
   --  Gets next character from file, which has already been checked for being
   --  in read status, and returns the character read if no error occurs. The
   --  result is EOF if the end of file was read.

   procedure Get_Character (File : File_Type; Item : out Character);
   --  This is essentially a copy of the normal Get routine from Text_IO. It
   --  obtains a single character from the input file File, and places it in
   --  Item. This character may be the leading character of a Wide_Character
   --  sequence, but that is up to the caller to deal with.

   function Get_Wide_Char
     (C    : Character;
      File : File_Type) return Wide_Character;
   --  This function is shared by Get and Get_Immediate to extract a wide
   --  character value from the given File. The first byte has already been
   --  read and is passed in C. The wide character value is returned as the
   --  result, and the file pointer is bumped past the character.

   function Nextc (File : File_Type) return Interfaces.C_Streams.int;
   --  Returns next character from file without skipping past it (i.e. it is a
   --  combination of Getc followed by an Ungetc).

end Ada.Wide_Text_IO;
