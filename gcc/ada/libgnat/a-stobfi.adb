------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_OUTPUT.BASIC_FILES                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

pragma Ada_2020;

with Ada.Strings.Text_Output.Utils; use Ada.Strings.Text_Output.Utils;
package body Ada.Strings.Text_Output.Basic_Files is
   use type OS.File_Descriptor;

   function Create_From_FD
     (FD : OS.File_Descriptor;
      Indent_Amount : Natural;
      Chunk_Length : Positive) return File;
   --  Create a file from an OS file descriptor

   function Create_From_FD
     (FD : OS.File_Descriptor;
      Indent_Amount : Natural;
      Chunk_Length : Positive) return File
   is
   begin
      if FD = OS.Invalid_FD then
         raise Program_Error with OS.Errno_Message;
      end if;
      return Result : File (Chunk_Length) do
         Result.Indent_Amount := Indent_Amount;
         Result.Cur_Chunk := Result.Initial_Chunk'Unchecked_Access;
         Result.FD := FD;
      end return;
   end Create_From_FD;

   function Create_File
     (Name : String;
      Indent_Amount : Natural := Default_Indent_Amount;
      Chunk_Length : Positive := Default_Chunk_Length) return File
   is
   begin
      return Create_From_FD
        (OS.Create_File (Name, Fmode => OS.Text),
         Indent_Amount, Chunk_Length);
   end Create_File;

   function Create_New_File
     (Name : String;
      Indent_Amount : Natural := Default_Indent_Amount;
      Chunk_Length : Positive := Default_Chunk_Length) return File
   is
   begin
      return Create_From_FD
        (OS.Create_New_File (Name, Fmode => OS.Text),
         Indent_Amount, Chunk_Length);
   end Create_New_File;

   procedure Close (S : in out File'Class) is
      Status : Boolean;
   begin
      Flush (S);

      if S.FD not in OS.Standout | OS.Standerr then -- Don't close these
         OS.Close (S.FD, Status);
         if not Status then
            raise Program_Error with OS.Errno_Message;
         end if;
      end if;
   end Close;

   overriding procedure Full_Method (S : in out File) renames Flush_Method;

   overriding procedure Flush_Method (S : in out File) is
      pragma Assert (S.Cur_Chunk = S.Initial_Chunk'Unchecked_Access);
      Res : constant Integer :=
        OS.Write (S.FD, S.Cur_Chunk.Chars'Address, S.Last);
   begin
      if Res /= S.Last then
         raise Program_Error with OS.Errno_Message;
      end if;
      S.Last := 0;
   end Flush_Method;

   The_Stdout : aliased File :=
     Create_From_FD (OS.Standout,
                     Indent_Amount => Default_Indent_Amount,
                     Chunk_Length => Default_Chunk_Length);
   The_Stderr : aliased File :=
     Create_From_FD (OS.Standerr,
                     Indent_Amount => Default_Indent_Amount,
                     Chunk_Length => Default_Chunk_Length);

   function Standard_Output return Sink_Access is (The_Stdout'Access);
   function Standard_Error return Sink_Access is (The_Stderr'Access);

end Ada.Strings.Text_Output.Basic_Files;
