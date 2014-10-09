------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           V X A D D R 2 L I N E                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This program is meant to be used with vxworks to compute symbolic
--  backtraces on the host from non-symbolic backtraces obtained on the target.

--  The basic idea is to automate the computation of the necessary address
--  adjustments prior to calling addr2line when the application has only been
--  partially linked on the host.

--  Variants for various targets are supported, and the command line should
--  be like :

--  <target>-addr2line [-a <target_arch>] <exe_file> <ref_address>
--                     <backtrace addresses>

--  Where:
--  <target_arch> :
--    selects the target architecture. In the absence of this parameter the
--    default variant is chosen based on the Detect_Arch result. Generally,
--    this parameter will only be used if vxaddr2line is recompiled manually.
--    Otherwise, the command name will always be of the form:
--      <target>-vxaddr2line
--    where there is no ambiguity on the target's architecture.

--  <exe_file> :
--    The name of the partially linked binary file for the application.

--  <ref_address> :
--    Runtime address (on the target) of a reference symbol you choose. This
--    name must match the value of the Ref_Symbol variable declared below.
--    A symbol with a small offset from the beginning of the text segment is
--    better, so "adainit" is a good choice.

--  <backtrace addresses> :
--    The call chain addresses you obtained at run time on the target and
--    for which you want a symbolic association.

--  TO ADD A NEW ARCHITECTURE add an appropriate value to Architecture type
--  (in a format <host>_<target>), and then an appropriate value to Config_List
--  array

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces;        use Interfaces;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Regpat;               use GNAT.Regpat;

procedure VxAddr2Line is

   package Unsigned_32_IO is new Modular_IO (Unsigned_32);
   --  Instantiate Modular_IO to have Put

   Ref_Symbol : constant String := "adainit";
   --  This is the name of the reference symbol whose runtime address must
   --  be provided as the <ref_address> argument.

   --  All supported architectures
   type Architecture is
     (DEC_ALPHA,
      LINUX_E500V2,
      LINUX_I586,
      LINUX_POWERPC,
      WINDOWS_E500V2,
      WINDOWS_I586,
      WINDOWS_M68K,
      WINDOWS_POWERPC,
      SOLARIS_E500V2,
      SOLARIS_I586,
      SOLARIS_POWERPC);

   type Arch_Record is record
      Addr2line_Binary : String_Access;
      --  Name of the addr2line utility to use

      Nm_Binary : String_Access;
      --  Name of the host nm utility, which will be used to find out the
      --  offset of the reference symbol in the text segment of the partially
      --  linked executable.

      Addr_Digits_To_Skip : Integer;
      --  When addresses such as 0xfffffc0001dfed50 are provided, for instance
      --  on ALPHA, indicate the number of leading digits that can be ignored,
      --  which will avoid computational overflows. Typically only useful when
      --  64bit addresses are provided.

      Bt_Offset_From_Call : Unsigned_32;
      --  Offset from a backtrace address to the address of the corresponding
      --  call instruction. This should always be 0, except on platforms where
      --  the backtrace addresses actually correspond to return and not call
      --  points. In such cases, a negative value is most likely.
   end record;

   --  Configuration for each of the architectures
   Arch_List : array (Architecture'Range) of Arch_Record :=
     (DEC_ALPHA =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 8,
         Bt_Offset_From_Call => 0),
      LINUX_E500V2 =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4),
      LINUX_I586 =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -2),
      LINUX_POWERPC =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4),
      SOLARIS_E500V2 =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4),
      SOLARIS_I586 =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -2),
      SOLARIS_POWERPC =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4),
      WINDOWS_E500V2 =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4),
      WINDOWS_I586 =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -2),
      WINDOWS_M68K =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4),
      WINDOWS_POWERPC =>
        (Addr2line_Binary    => null,
         Nm_Binary           => null,
         Addr_Digits_To_Skip => 0,
         Bt_Offset_From_Call => -4)
     );

   --  Current architecture
   Cur_Arch : Architecture;

   --  State of architecture detection
   Detect_Success : Boolean := False;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Error (Msg : String);
   pragma No_Return (Error);
   --  Prints the message and then terminates the program

   procedure Usage;
   --  Displays the short help message and then terminates the program

   function Get_Reference_Offset return Unsigned_32;
   --  Computes the static offset of the reference symbol by calling nm

   function Get_Value_From_Hex_Arg (Arg : Natural) return Unsigned_32;
   --  Threats the argument number Arg as a C-style hexadecimal literal
   --  and returns its integer value

   function Hex_Image (Value : Unsigned_32) return String_Access;
   --  Returns access to a string that contains hexadecimal image of Value

   --  Separate functions that provide build-time customization:

   procedure Detect_Arch;
   --  Saves in Cur_Arch the current architecture, based on the name of
   --  vxaddr2line instance and properties of the host. Detect_Success is False
   --  if detection fails

   -----------------
   -- Detect_Arch --
   -----------------

   procedure Detect_Arch is
      Name   : constant String := Base_Name (Command_Name);
      Proc   : constant String :=
                 Name (Name'First .. Index (Name, "-") - 1);
      Target : constant String :=
                 Name (Name'First .. Index (Name, "vxaddr2line") - 1);

   begin
      Detect_Success := False;

      if Proc = "" then
         return;
      end if;

      if Proc = "alpha" then
         Cur_Arch := DEC_ALPHA;
      else
         --  Let's detect the host.
         --  ??? A naive implementation that can't distinguish between Unixes
         if Directory_Separator = '/' then
            Cur_Arch := Architecture'Value ("solaris_" & Proc);
         else
            Cur_Arch := Architecture'Value ("windows_" & Proc);
         end if;
      end if;

      if Arch_List (Cur_Arch).Addr2line_Binary = null then
         Arch_List (Cur_Arch).Addr2line_Binary := new String'
           (Target & "addr2line");
      end if;
      if Arch_List (Cur_Arch).Nm_Binary = null then
         Arch_List (Cur_Arch).Nm_Binary := new String'
           (Target & "nm");
      end if;

      Detect_Success := True;

   exception
      when others =>
         return;
   end Detect_Arch;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Msg);
      OS_Exit (1);
      raise Program_Error;
   end Error;

   --------------------------
   -- Get_Reference_Offset --
   --------------------------

   function Get_Reference_Offset return Unsigned_32 is
      Nm_Cmd  : constant String_Access :=
                  Locate_Exec_On_Path (Arch_List (Cur_Arch).Nm_Binary.all);

      Nm_Args : constant Argument_List :=
                  (new String'("-P"),
                   new String'(Argument (1)));

      Forever   : aliased String := "^@@@@";
      Reference : aliased String := Ref_Symbol & "\s+\S\s+([\da-fA-F]+)";

      Pd     : Process_Descriptor;
      Result : Expect_Match;

   begin
      --  If Nm is not found, abort

      if Nm_Cmd = null then
         Error ("Couldn't find " & Arch_List (Cur_Arch).Nm_Binary.all);
      end if;

      Non_Blocking_Spawn
        (Pd, Nm_Cmd.all, Nm_Args, Buffer_Size => 0, Err_To_Out => True);

      --  Expect a string containing the reference symbol

      Expect (Pd, Result,
              Regexp_Array'(1 => Reference'Unchecked_Access),
              Timeout => -1);

      --  If we are here, the pattern was matched successfully

      declare
         Match_String : constant String := Expect_Out_Match (Pd);
         Matches      : Match_Array (0 .. 1);
         Value        : Unsigned_32;

      begin
         Match (Reference, Match_String, Matches);
         Value := Unsigned_32'Value
           ("16#"
            & Match_String (Matches (1).First .. Matches (1).Last) & "#");

         --  Expect a string that will never be emitted, so that the
         --  process can be correctly terminated (with Process_Died)

         Expect (Pd, Result,
                 Regexp_Array'(1 => Forever'Unchecked_Access),
                 Timeout => -1);

      exception
         when Process_Died =>
            return Value;
      end;

      --  We cannot get here

      raise Program_Error;

   exception
      when Invalid_Process =>
         Error ("Could not spawn a process " & Nm_Cmd.all);

      when others    =>

         --  The process died without matching the reference symbol or the
         --  format wasn't recognized.

         Error ("Unexpected output from " & Nm_Cmd.all);
   end Get_Reference_Offset;

   ----------------------------
   -- Get_Value_From_Hex_Arg --
   ----------------------------

   function Get_Value_From_Hex_Arg (Arg : Natural) return Unsigned_32 is
      Cur_Arg : constant String := Argument (Arg);
      Offset  : Natural;

   begin
      --  Skip "0x" prefix if present

      if Cur_Arg'Length > 2 and then Cur_Arg (1 .. 2) = "0x" then
         Offset := 3;
      else
         Offset := 1;
      end if;

      --  Add architecture-specific offset

      Offset := Offset + Arch_List (Cur_Arch).Addr_Digits_To_Skip;

      --  Convert to value

      return Unsigned_32'Value
        ("16#" & Cur_Arg (Offset .. Cur_Arg'Last) & "#");

   exception
      when Constraint_Error =>

         Error ("Can't parse backtrace address '" & Cur_Arg & "'");
         raise;
   end Get_Value_From_Hex_Arg;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : Unsigned_32) return String_Access is
      Result    : String (1 .. 20);
      Start_Pos : Natural;

   begin
      Unsigned_32_IO.Put (Result, Value, 16);
      Start_Pos := Index (Result, "16#") + 3;
      return new String'(Result (Start_Pos .. Result'Last - 1));
   end Hex_Image;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage : " & Base_Name (Command_Name)
                & " <executable> <"
                & Ref_Symbol & " offset on target> <addr1> ...");

      OS_Exit (1);
   end Usage;

   Ref_Static_Offset, Ref_Runtime_Address, Bt_Address : Unsigned_32;

   Addr2line_Cmd : String_Access;

   Addr2line_Args : Argument_List (1 .. 501);
   --  We expect that there won't be more than 500 backtrace frames

   Addr2line_Args_Count : Natural;

   Success : Boolean;

--  Start of processing for VxAddr2Line

begin

   Detect_Arch;

   --  There should be at least two arguments

   if Argument_Count < 2 then
      Usage;
   end if;

   --  Enforce HARD LIMIT There should be at most 501 arguments. Why 501???

   if Argument_Count > 501 then
      Error ("Too many backtrace frames");
   end if;

   --  Do we have a valid architecture?

   if not Detect_Success then
      Put_Line ("Couldn't detect the architecture");
      return;
   end if;

   Addr2line_Cmd :=
     Locate_Exec_On_Path (Arch_List (Cur_Arch).Addr2line_Binary.all);

   --  If Addr2line is not found, abort

   if Addr2line_Cmd = null then
      Error ("Couldn't find " & Arch_List (Cur_Arch).Addr2line_Binary.all);
   end if;

   --  The first argument specifies the image file. Check if it exists

   if not Is_Regular_File (Argument (1)) then
      Error ("Couldn't find the executable " & Argument (1));
   end if;

   --  The second argument specifies the reference symbol runtime address.
   --  Let's parse and store it

   Ref_Runtime_Address := Get_Value_From_Hex_Arg (2);

   --  Run nm command to get the reference symbol static offset

   Ref_Static_Offset := Get_Reference_Offset;

   --  Build addr2line parameters. First, the standard part

   Addr2line_Args (1) := new String'("--exe=" & Argument (1));
   Addr2line_Args_Count := 1;

   --  Now, append to this the adjusted backtraces in arguments 4 and further

   for J in 3 .. Argument_Count loop

      --  Basically, for each address in the runtime backtrace ...

      --  o We compute its offset relatively to the runtime address of the
      --    reference symbol,

      --  and then ...

      --  o We add this offset to the static one for the reference symbol in
      --    the executable to find the executable offset corresponding to the
      --    backtrace address.

      Bt_Address := Get_Value_From_Hex_Arg (J);

      Bt_Address :=
        Bt_Address - Ref_Runtime_Address
                   + Ref_Static_Offset
                   + Arch_List (Cur_Arch).Bt_Offset_From_Call;

      Addr2line_Args_Count := Addr2line_Args_Count + 1;
      Addr2line_Args (Addr2line_Args_Count) := Hex_Image (Bt_Address);
   end loop;

   --  Run the resulting command

   Spawn (Addr2line_Cmd.all,
          Addr2line_Args (1 .. Addr2line_Args_Count), Success);

   if not Success then
      Error ("Couldn't spawn " & Addr2line_Cmd.all);
   end if;

exception
   when others =>

      --  Mask all exceptions

      return;
end VxAddr2Line;
