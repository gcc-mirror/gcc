------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               V X L I N K                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

pragma Ada_2012;

with Ada.Command_Line;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

package body VxLink is

   Target_Triplet : Unbounded_String := Null_Unbounded_String;
   Verbose        : Boolean := False;
   Error_State    : Boolean := False;

   function Triplet return String;
   --  ??? missing spec

   function Which (Exe : String) return String;
   --  ??? missing spec

   -------------
   -- Triplet --
   -------------

   function Triplet return String is
   begin
      if Target_Triplet = Null_Unbounded_String then
         declare
            Exe : constant String := File_Name (Ada.Command_Line.Command_Name);
         begin
            for J in reverse Exe'Range loop
               if Exe (J) = '-' then
                  Target_Triplet := To_Unbounded_String (Exe (Exe'First .. J));
                  exit;
               end if;
            end loop;
         end;
      end if;

      return To_String (Target_Triplet);
   end Triplet;

   -----------
   -- Which --
   -----------

   function Which (Exe : String) return String is
      Suffix   : GNAT.OS_Lib.String_Access := Get_Executable_Suffix;
      Basename : constant String := Exe & Suffix.all;
      Path     : GNAT.OS_Lib.String_Access := Getenv ("PATH");
      Last     : Natural := Path'First;

   begin
      Free (Suffix);

      for J in Path'Range loop
         if Path (J) = Path_Separator then
            declare
               Full : constant String := Normalize_Pathname
                 (Name           => Basename,
                  Directory      => Path (Last .. J - 1),
                  Resolve_Links  => False,
                  Case_Sensitive => True);
            begin
               if Is_Executable_File (Full) then
                  Free (Path);

                  return Full;
               end if;
            end;

            Last := J + 1;
         end if;
      end loop;

      Free (Path);

      return "";
   end Which;

   -----------------
   -- Set_Verbose --
   -----------------

   procedure Set_Verbose (Value : Boolean) is
   begin
      Verbose := Value;
   end Set_Verbose;

   ----------------
   -- Is_Verbose --
   ----------------

   function Is_Verbose return Boolean is
   begin
      return Verbose;
   end Is_Verbose;

   ---------------------
   -- Set_Error_State --
   ---------------------

   procedure Set_Error_State (Message : String) is
   begin
      Log_Error ("Error: " & Message);
      Error_State := True;
      Ada.Command_Line.Set_Exit_Status (1);
   end Set_Error_State;

   --------------------
   -- Is_Error_State --
   --------------------

   function Is_Error_State return Boolean is
   begin
      return Error_State;
   end Is_Error_State;

   --------------
   -- Log_Info --
   --------------

   procedure Log_Info (S : String) is
   begin
      if Verbose then
         Ada.Text_IO.Put_Line (S);
      end if;
   end Log_Info;

   ---------------
   -- Log_Error --
   ---------------

   procedure Log_Error (S : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, S);
   end Log_Error;

   ---------
   -- Run --
   ---------

   procedure Run (Arguments : Arguments_List) is
      Output : constant String := Run (Arguments);
   begin
      if not Is_Error_State then
         --  In case of erroneous execution, the function version of run will
         --  have already displayed the output
         Ada.Text_IO.Put (Output);
      end if;
   end Run;

   ---------
   -- Run --
   ---------

   function Run (Arguments : Arguments_List) return String is
      Args       : GNAT.OS_Lib.Argument_List_Access :=
                     new GNAT.OS_Lib.Argument_List
                       (1 .. Natural (Arguments.Length) - 1);
      Base       : constant String := Base_Name (Arguments.First_Element);

      Debug_Line : Unbounded_String;
      Add_Quotes : Boolean;

   begin
      if Verbose then
         Append (Debug_Line, Base);
      end if;

      for J in Arguments.First_Index + 1 .. Arguments.Last_Index loop
         declare
            Arg : String renames Arguments.Element (J);
         begin
            Args (J - 1) := new String'(Arg);

            if Verbose then
               Add_Quotes := False;

               for K in Arg'Range loop
                  if Arg (K) = ' ' then
                     Add_Quotes := True;
                     exit;
                  end if;
               end loop;

               Append (Debug_Line, ' ');

               if Add_Quotes then
                  Append (Debug_Line, '"' & Arg & '"');
               else
                  Append (Debug_Line, Arg);
               end if;
            end if;
         end;
      end loop;

      if Verbose then
         Ada.Text_IO.Put_Line (To_String (Debug_Line));
      end if;

      declare
         Status : aliased Integer := 0;
         Ret    : constant String :=
                    Get_Command_Output
                      (Command    => Arguments.First_Element,
                       Arguments  => Args.all,
                       Input      => "",
                       Status     => Status'Access,
                       Err_To_Out => True);

      begin
         GNAT.OS_Lib.Free (Args);

         if Status /= 0 then
            pragma Annotate (Codepeer, False_Positive,
                             "test always false",
                             "Status modified by Get_Command_Output");

            Ada.Text_IO.Put_Line (Ret);
            Set_Error_State
              (Base_Name (Arguments.First_Element) &
                 " returned" & Status'Image);
         end if;

         return Ret;
      end;
   end Run;

   ---------
   -- Gcc --
   ---------

   function Gcc return String is
   begin
      return Which (Triplet & "gcc");
   end Gcc;

   ---------
   -- Gxx --
   ---------

   function Gxx return String is
   begin
      return Which (Triplet & "g++");
   end Gxx;

   --------
   -- Nm --
   --------

   function Nm return String is
   begin
      return Which (Triplet & "nm");
   end Nm;

end VxLink;
