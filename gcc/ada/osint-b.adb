------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - B                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
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

with Hostparm;
with Namet;   use Namet;
with Opt;     use Opt;

package body Osint.B is

   Binder_Output_Time_Stamps_Set : Boolean := False;

   Old_Binder_Output_Time_Stamp  : Time_Stamp_Type;
   New_Binder_Output_Time_Stamp  : Time_Stamp_Type;
   Recording_Time_From_Last_Bind : Boolean := False;

   -------------------------
   -- Close_Binder_Output --
   -------------------------

   procedure Close_Binder_Output is
      Status : Boolean;
   begin
      Close (Output_FD, Status);

      if not Status then
         Fail
           ("error while closing generated file ",
            Get_Name_String (Output_File_Name));
      end if;

      if Recording_Time_From_Last_Bind then
         New_Binder_Output_Time_Stamp  := File_Stamp (Output_File_Name);
         Binder_Output_Time_Stamps_Set := True;
      end if;
   end Close_Binder_Output;

   --------------------------
   -- Create_Binder_Output --
   --------------------------

   procedure Create_Binder_Output
     (Output_File_Name : String;
      Typ              : Character;
      Bfile            : out Name_Id)
   is
      File_Name : String_Ptr;
      Findex1   : Natural;
      Findex2   : Natural;
      Flength   : Natural;

   begin
      if Output_File_Name /= "" then
         Name_Buffer (Output_File_Name'Range) := Output_File_Name;
         Name_Buffer (Output_File_Name'Last + 1) := ASCII.NUL;

         if Typ = 's' then
            Name_Buffer (Output_File_Name'Last) := 's';
         end if;

         Name_Len := Output_File_Name'Last;

      else
         Name_Buffer (1) := 'b';
         File_Name := File_Names (Current_File_Name_Index);

         Findex1 := File_Name'First;

         --  The ali file might be specified by a full path name. However,
         --  the binder generated file should always be created in the
         --  current directory, so the path might need to be stripped away.
         --  In addition to the default directory_separator allow the '/' to
         --  act as separator since this is allowed in MS-DOS and OS2 ports.

         for J in reverse File_Name'Range loop
            if File_Name (J) = Directory_Separator
              or else File_Name (J) = '/'
            then
               Findex1 := J + 1;
               exit;
            end if;
         end loop;

         Findex2 := File_Name'Last;
         while File_Name (Findex2) /=  '.' loop
            Findex2 := Findex2 - 1;
         end loop;

         Flength := Findex2 - Findex1;

         if Maximum_File_Name_Length > 0 then

            --  Make room for the extra two characters in "b?"

            while Int (Flength) > Maximum_File_Name_Length - 2 loop
               Findex2 := Findex2 - 1;
               Flength := Findex2 - Findex1;
            end loop;
         end if;

         Name_Buffer (3 .. Flength + 2) := File_Name (Findex1 .. Findex2 - 1);
         Name_Buffer (Flength + 3) := '.';

         --  C bind file, name is b_xxx.c

         if Typ = 'c' then
            Name_Buffer (2) := '_';
            Name_Buffer (Flength + 4) := 'c';
            Name_Buffer (Flength + 5) := ASCII.NUL;
            Name_Len := Flength + 4;

         --  Ada bind file, name is b~xxx.adb or b~xxx.ads
         --  (with $ instead of ~ in VMS)

         else
            if Hostparm.OpenVMS then
               Name_Buffer (2) := '$';
            else
               Name_Buffer (2) := '~';
            end if;

            Name_Buffer (Flength + 4) := 'a';
            Name_Buffer (Flength + 5) := 'd';
            Name_Buffer (Flength + 6) := Typ;
            Name_Buffer (Flength + 7) := ASCII.NUL;
            Name_Len := Flength + 6;
         end if;
      end if;

      Bfile := Name_Find;

      if Recording_Time_From_Last_Bind then
         Old_Binder_Output_Time_Stamp := File_Stamp (Bfile);
      end if;

      Create_File_And_Check (Output_FD, Text);
   end Create_Binder_Output;

   --------------------
   -- More_Lib_Files --
   --------------------

   function More_Lib_Files return Boolean renames  More_Files;

   ------------------------
   -- Next_Main_Lib_File --
   ------------------------

   function Next_Main_Lib_File return File_Name_Type renames Next_Main_File;

   --------------------------------
   -- Record_Time_From_Last_Bind --
   --------------------------------

   procedure Record_Time_From_Last_Bind is
   begin
      Recording_Time_From_Last_Bind := True;
   end Record_Time_From_Last_Bind;

   -------------------------
   -- Time_From_Last_Bind --
   -------------------------

   function Time_From_Last_Bind return Nat is
      Old_Y  : Nat;
      Old_M  : Nat;
      Old_D  : Nat;
      Old_H  : Nat;
      Old_Mi : Nat;
      Old_S  : Nat;
      New_Y  : Nat;
      New_M  : Nat;
      New_D  : Nat;
      New_H  : Nat;
      New_Mi : Nat;
      New_S  : Nat;

      type Month_Data is array (Int range 1 .. 12) of Int;
      Cumul : constant Month_Data := (0, 0, 3, 3, 4, 4, 5, 5, 5, 6, 6, 7);
      --  Represents the difference in days from a period compared to the
      --  same period if all months had 31 days, i.e:
      --
      --    Cumul (m) = 31x(m-1) - (number of days from 01/01 to m/01)

      Res : Int;

   begin
      if not Recording_Time_From_Last_Bind
        or else not Binder_Output_Time_Stamps_Set
        or else Old_Binder_Output_Time_Stamp = Empty_Time_Stamp
      then
         return Nat'Last;
      end if;

      Split_Time_Stamp
       (Old_Binder_Output_Time_Stamp,
        Old_Y, Old_M, Old_D, Old_H, Old_Mi, Old_S);

      Split_Time_Stamp
       (New_Binder_Output_Time_Stamp,
        New_Y, New_M, New_D, New_H, New_Mi, New_S);

      Res := New_Mi - Old_Mi;

      --  60 minutes in an hour

      Res := Res + 60 * (New_H  - Old_H);

      --  24 hours in a day

      Res := Res + 60 * 24 * (New_D  - Old_D);

      --  Almost 31 days in a month

      Res := Res + 60 * 24 *
        (31 * (New_M - Old_M) - Cumul (New_M) + Cumul (Old_M));

      --  365 days in a year

      Res := Res + 60 * 24 * 365 * (New_Y - Old_Y);

      return Res;
   end Time_From_Last_Bind;

   -----------------------
   -- Write_Binder_Info --
   -----------------------

   procedure Write_Binder_Info (Info : String) renames Write_Info;

begin
   Set_Program (Binder);
end Osint.B;
