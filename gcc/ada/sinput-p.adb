------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Opt;         use Opt;
with System;      use System;

package body Sinput.P is

   First : Boolean := True;
   --  Flag used when Load_Project_File is called the first time,
   --  to set Main_Source_File.
   --  The flag is reset to False at the first call to Load_Project_File

   -----------------------
   -- Load_Project_File --
   -----------------------

   function Load_Project_File (Path : String) return Source_File_Index is
      Src  : Source_Buffer_Ptr;
      X    : Source_File_Index;
      Lo   : Source_Ptr;
      Hi   : Source_Ptr;

      Source_File_FD : File_Descriptor;
      --  The file descriptor for the current source file. A negative value
      --  indicates failure to open the specified source file.

      Len : Integer;
      --  Length of file. Assume no more than 2 gigabytes of source!

      Actual_Len : Integer;

      Path_Id : Name_Id;
      File_Id : Name_Id;

   begin
      if Path = "" then
         return No_Source_File;
      end if;

      Source_File.Increment_Last;
      X := Source_File.Last;

      if First then
         Main_Source_File := X;
         First := False;
      end if;

      if X = Source_File.First then
         Lo := First_Source_Ptr;
      else
         Lo := Source_File.Table (X - 1).Source_Last + 1;
      end if;

      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      Path_Id := Name_Find;
      Name_Buffer (Name_Len + 1) := ASCII.NUL;

      --  Open the source FD, note that we open in binary mode, because as
      --  documented in the spec, the caller is expected to handle either
      --  DOS or Unix mode files, and there is no point in wasting time on
      --  text translation when it is not required.

      Source_File_FD := Open_Read (Name_Buffer'Address, Binary);

      if Source_File_FD = Invalid_FD then
         Source_File.Decrement_Last;
         return No_Source_File;

      end if;

      Len := Integer (File_Length (Source_File_FD));

      --  Set Hi so that length is one more than the physical length,
      --  allowing for the extra EOF character at the end of the buffer

      Hi := Lo + Source_Ptr (Len);

      --  Do the actual read operation

      declare
         subtype Actual_Source_Buffer is Source_Buffer (Lo .. Hi);
         --  Physical buffer allocated

         type Actual_Source_Ptr is access Actual_Source_Buffer;
         --  This is the pointer type for the physical buffer allocated

         Actual_Ptr : Actual_Source_Ptr := new Actual_Source_Buffer;
         --  And this is the actual physical buffer

      begin
         --  Allocate source buffer, allowing extra character at end for EOF

         --  Some systems (e.g. VMS) have file types that require one
         --  read per line, so read until we get the Len bytes or until
         --  there are no more characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Source_File_FD, Actual_Ptr (Hi)'Address, Len);
            Hi := Hi + Source_Ptr (Actual_Len);
            exit when Actual_Len = Len or Actual_Len <= 0;
         end loop;

         Actual_Ptr (Hi) := EOF;

         --  Now we need to work out the proper virtual origin pointer to
         --  return. This is exactly Actual_Ptr (0)'Address, but we have
         --  to be careful to suppress checks to compute this address.

         declare
            pragma Suppress (All_Checks);

            function To_Source_Buffer_Ptr is new
              Ada.Unchecked_Conversion (Address, Source_Buffer_Ptr);

         begin
            Src := To_Source_Buffer_Ptr (Actual_Ptr (0)'Address);
         end;
      end;

      --  Read is complete, get time stamp and close file and we are done

      Close (Source_File_FD);

      --  Get the file name, without path information

      declare
         Index : Positive := Path'Last;

      begin
         while Index > Path'First loop
            exit when Path (Index - 1) = '/';
            exit when Path (Index - 1) = Directory_Separator;
            Index := Index - 1;
         end loop;

         Name_Len := Path'Last - Index + 1;
         Name_Buffer (1 .. Name_Len) := Path (Index .. Path'Last);
         File_Id := Name_Find;
      end;

      declare
         S : Source_File_Record renames Source_File.Table (X);

      begin
         S := (Debug_Source_Name   => Path_Id,
               File_Name           => File_Id,
               First_Mapped_Line   => No_Line_Number,
               Full_File_Name      => Path_Id,
               Full_Ref_Name       => Path_Id,
               Identifier_Casing   => Unknown,
               Instantiation       => No_Location,
               Keyword_Casing      => Unknown,
               Last_Source_Line    => 1,
               License             => Unknown,
               Lines_Table         => null,
               Lines_Table_Max     => 1,
               Logical_Lines_Table => null,
               Num_SRef_Pragmas    => 0,
               Reference_Name      => File_Id,
               Sloc_Adjust         => 0,
               Source_Checksum     => 0,
               Source_First        => Lo,
               Source_Last         => Hi,
               Source_Text         => Src,
               Template            => No_Source_File,
               Time_Stamp          => Empty_Time_Stamp);

         Alloc_Line_Tables (S, Opt.Table_Factor * Alloc.Lines_Initial);
         S.Lines_Table (1) := Lo;
      end;

      return X;
   end Load_Project_File;

   --------------------------------
   -- Restore_Project_Scan_State --
   --------------------------------

   procedure Restore_Project_Scan_State
     (Saved_State : in Saved_Project_Scan_State)
   is
   begin
      Restore_Scan_State (Saved_State.Scan_State);
      Source              := Saved_State.Source;
      Current_Source_File := Saved_State.Current_Source_File;
   end Restore_Project_Scan_State;

   -----------------------------
   -- Save_Project_Scan_State --
   -----------------------------

   procedure Save_Project_Scan_State
     (Saved_State : out Saved_Project_Scan_State)
   is
   begin
      Save_Scan_State (Saved_State.Scan_State);
      Saved_State.Source              := Source;
      Saved_State.Current_Source_File := Current_Source_File;
   end Save_Project_Scan_State;

end Sinput.P;
