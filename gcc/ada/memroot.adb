------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M E M R O O T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $
--                                                                          --
--            Copyright (C) 1997-2001 Ada Core Technologies, Inc.           --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Table;
with GNAT.HTable; use GNAT.HTable;
with Ada.Text_IO; use Ada.Text_IO;

package body Memroot is

   -------------
   -- Name_Id --
   -------------

   package Chars is new GNAT.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10_000,
     Table_Increment      => 100);
   --  The actual character container for names

   type Name is  record
      First, Last : Integer;
   end record;

   package Names is new GNAT.Table (
     Table_Component_Type => Name,
     Table_Index_Type     => Name_Id,
     Table_Low_Bound      => 0,
     Table_Initial        => 400,
     Table_Increment      => 100);

   type Name_Range is range 1 .. 1023;

   function Name_Eq (N1, N2 : Name) return Boolean;
   --  compare 2 names

   function H (N : Name) return Name_Range;

   package Name_HTable is new GNAT.HTable.Simple_HTable (
     Header_Num => Name_Range,
     Element    => Name_Id,
     No_Element => No_Name_Id,
     Key        => Name,
     Hash       => H,
     Equal      => Name_Eq);

   --------------
   -- Frame_Id --
   --------------

   type Frame is record
      Name, File, Line : Name_Id;
   end record;

   function Image
     (F       : Frame_Id;
      Max_Fil : Integer;
      Max_Lin : Integer)
      return String;
   --  Returns an image for F containing the file name, the Line number,
   --  and the subprogram name. When possible, spaces are inserted between
   --  the line number and the subprogram name in order to align images of the
   --  same frame. Alignement is cimputed with Max_Fil & Max_Lin representing
   --  the max number of character in a filename or length in a given frame.

   package Frames is new GNAT.Table (
     Table_Component_Type => Frame,
     Table_Index_Type     => Frame_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 400,
     Table_Increment      => 100);

   type Frame_Range is range 1 .. 513;
   function H (N : Frame) return Frame_Range;

   package Frame_HTable is new GNAT.HTable.Simple_HTable (
     Header_Num => Frame_Range,
     Element    => Frame_Id,
     No_Element => No_Frame_Id,
     Key        => Frame,
     Hash       => H,
     Equal      => "=");

   -------------
   -- Root_Id --
   -------------

   type Root is  record
     First, Last     : Integer;
     Nb_Alloc        : Integer;
     Alloc_Size      : Storage_Count;
     High_Water_Mark : Storage_Count;
   end record;

   package Frames_In_Root is new GNAT.Table (
     Table_Component_Type => Frame_Id,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 400,
     Table_Increment      => 100);

   package Roots is new GNAT.Table (
     Table_Component_Type => Root,
     Table_Index_Type     => Root_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 100);
   type Root_Range is range 1 .. 513;

   function Root_Eq (N1, N2 : Root) return Boolean;
   function H     (B : Root)     return Root_Range;

   package Root_HTable is new GNAT.HTable.Simple_HTable (
     Header_Num => Root_Range,
     Element    => Root_Id,
     No_Element => No_Root_Id,
     Key        => Root,
     Hash       => H,
     Equal      => Root_Eq);

   ----------------
   -- Alloc_Size --
   ----------------

   function Alloc_Size (B : Root_Id) return Storage_Count is
   begin
      return Roots.Table (B).Alloc_Size;
   end Alloc_Size;

   -----------------
   -- Enter_Frame --
   -----------------

   function Enter_Frame (Name, File, Line : Name_Id) return Frame_Id is
      Res   : Frame_Id;

   begin
      Frames.Increment_Last;
      Frames.Table (Frames.Last) := Frame'(Name, File, Line);
      Res := Frame_HTable.Get (Frames.Table (Frames.Last));

      if Res /= No_Frame_Id then
         Frames.Decrement_Last;
         return Res;

      else
         Frame_HTable.Set (Frames.Table (Frames.Last), Frames.Last);
         return Frames.Last;
      end if;
   end Enter_Frame;

   ----------------
   -- Enter_Name --
   ----------------

   function Enter_Name (S : String) return Name_Id is
      Old_L : constant Integer := Chars.Last;
      Len   : constant Integer := S'Length;
      F     : constant Integer := Chars.Allocate (Len);
      Res   : Name_Id;

   begin
      Chars.Table (F .. F + Len - 1) := Chars.Table_Type (S);
      Names.Increment_Last;
      Names.Table (Names.Last) := Name'(F, F + Len - 1);
      Res := Name_HTable.Get (Names.Table (Names.Last));

      if Res /= No_Name_Id then
         Names.Decrement_Last;
         Chars.Set_Last (Old_L);
         return Res;

      else
         Name_HTable.Set (Names.Table (Names.Last), Names.Last);
         return Names.Last;
      end if;
   end Enter_Name;

   ----------------
   -- Enter_Root --
   ----------------

   function Enter_Root (Fr : Frame_Array) return Root_Id is
      Old_L : constant Integer  := Frames_In_Root.Last;
      Len   : constant Integer  := Fr'Length;
      F     : constant Integer  := Frames_In_Root.Allocate (Len);
      Res   : Root_Id;

   begin
      Frames_In_Root.Table (F .. F + Len - 1) :=
        Frames_In_Root.Table_Type (Fr);
      Roots.Increment_Last;
      Roots.Table (Roots.Last) := Root'(F, F + Len - 1, 0, 0, 0);
      Res := Root_HTable.Get (Roots.Table (Roots.Last));

      if Res /= No_Root_Id then
         Frames_In_Root.Set_Last (Old_L);
         Roots.Decrement_Last;
         return Res;

      else
         Root_HTable.Set (Roots.Table (Roots.Last), Roots.Last);
         return Roots.Last;
      end if;
   end Enter_Root;

   ---------------
   -- Frames_Of --
   ---------------

   function Frames_Of (B : Root_Id) return Frame_Array is
   begin
      return Frame_Array (
        Frames_In_Root.Table (Roots.Table (B).First .. Roots.Table (B).Last));
   end Frames_Of;

   ---------------
   -- Get_First --
   ---------------

   function Get_First return Root_Id is
   begin
      return  Root_HTable.Get_First;
   end Get_First;

   --------------
   -- Get_Next --
   --------------

   function Get_Next return Root_Id is
   begin
      return Root_HTable.Get_Next;
   end Get_Next;

   -------
   -- H --
   -------

   function H (B : Root) return Root_Range is

      type Uns is mod 2 ** 32;

      function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Uns := 0;

   begin
      for J in B.First .. B.Last loop
         Tmp := Rotate_Left (Tmp, 1) + Uns (Frames_In_Root.Table (J));
      end loop;

      return Root_Range'First
        + Root_Range'Base (Tmp mod Root_Range'Range_Length);
   end H;

   function H (N : Name) return Name_Range is
      function H is new Hash (Name_Range);

   begin
      return H (String (Chars.Table (N.First .. N.Last)));
   end H;

   function H (N : Frame) return Frame_Range is
   begin
      return Frame_Range (1 + (7 * N.Name + 13 * N.File + 17 * N.Line)
                                mod Frame_Range'Range_Length);
   end H;

   ---------------------
   -- High_Water_Mark --
   ---------------------

   function High_Water_Mark (B : Root_Id) return Storage_Count is
   begin
      return Roots.Table (B).High_Water_Mark;
   end High_Water_Mark;

   -----------
   -- Image --
   -----------

   function Image (N : Name_Id) return String is
      Nam : Name renames Names.Table (N);

   begin
      return String (Chars.Table (Nam.First .. Nam.Last));
   end Image;

   function Image
     (F       : Frame_Id;
      Max_Fil : Integer;
      Max_Lin : Integer)
      return String is

      Fram : Frame renames Frames.Table (F);
      Fil  : Name renames Names.Table (Fram.File);
      Lin  : Name renames Names.Table (Fram.Line);
      Nam  : Name renames Names.Table (Fram.Name);

      Fil_Len  : constant Integer := Fil.Last - Fil.First + 1;
      Lin_Len  : constant Integer := Lin.Last - Lin.First + 1;

      use type Chars.Table_Type;

      Spaces : constant String (1 .. 80) := (1 .. 80 => ' ');

   begin
      return String (Chars.Table (Fil.First .. Fil.Last))
        & ':'
        & String (Chars.Table (Lin.First .. Lin.Last))
        & Spaces (1 .. 1 + Max_Fil - Fil_Len + Max_Lin - Lin_Len)
        & String (Chars.Table (Nam.First .. Nam.Last));
   end Image;

   -------------
   -- Name_Eq --
   -------------

   function Name_Eq (N1, N2 : Name) return Boolean is
      use type Chars.Table_Type;
   begin
      return
        Chars.Table (N1.First .. N1.Last) = Chars.Table (N2.First .. N2.Last);
   end Name_Eq;

   --------------
   -- Nb_Alloc --
   --------------

   function Nb_Alloc (B : Root_Id) return Integer is
   begin
      return Roots.Table (B).Nb_Alloc;
   end Nb_Alloc;

   --------------
   -- Print_BT --
   --------------

   procedure Print_BT (B  : Root_Id) is
      Max_Col_Width : constant := 35;
      --  Largest filename length for which backtraces will be
      --  properly aligned. Frames containing longer names won't be
      --  truncated but they won't be properly aligned either.

      F : constant Frame_Array := Frames_Of (B);

      Max_Fil : Integer;
      Max_Lin : Integer;

   begin
      Max_Fil := 0;
      Max_Lin := 0;

      for J in F'Range loop
         declare
            Fram : Frame renames Frames.Table (F (J));
            Fil  : Name renames Names.Table (Fram.File);
            Lin  : Name renames Names.Table (Fram.Line);

         begin
            Max_Fil := Integer'Max (Max_Fil, Fil.Last - Fil.First + 1);
            Max_Lin := Integer'Max (Max_Lin, Lin.Last - Lin.First + 1);
         end;
      end loop;

      Max_Fil := Integer'Min (Max_Fil, Max_Col_Width);

      for J in F'Range loop
         Put ("   ");
         Put_Line (Image (F (J), Max_Fil, Max_Lin));
      end loop;
   end Print_BT;

   -------------
   -- Read_BT --
   -------------

   function Read_BT (BT_Depth : Integer; FT : File_Type) return Root_Id is
      Max_Line : constant Integer := 500;
      Curs1    : Integer;
      Curs2    : Integer;
      Line     : String (1 .. Max_Line);
      Last     : Integer := 0;
      Frames   : Frame_Array (1 .. BT_Depth);
      F        : Integer := Frames'First;
      Nam      : Name_Id;
      Fil      : Name_Id;
      Lin      : Name_Id;

      No_File    : Boolean := False;
      Main_Found : Boolean := False;

      procedure Find_File;
      --  Position Curs1 and Curs2 so that Line (Curs1 .. Curs2) contains
      --  the file name. The file name may not be on the current line since
      --  a frame may be printed on more than one line when there is a lot
      --  of parameters or names are long, so this subprogram can read new
      --  lines of input.

      procedure Find_Line;
      --  Position Curs1 and Curs2 so that Line (Curs1 .. Curs2) contains
      --  the line number.

      procedure Find_Name;
      --  Position Curs1 and Curs2 so that Line (Curs1 .. Curs2) contains
      --  the subprogram name.

      procedure Gmem_Read_BT_Frame (Buf : out String; Last : out Natural);
      --  GMEM functionality binding

      ---------------
      -- Find_File --
      ---------------

      procedure Find_File is
         Match_Parent : Integer;

      begin
         --  Skip parameters

         Curs1 := Curs2 + 3;
         Match_Parent := 1;
         while Curs1 <= Last loop
            if Line (Curs1) = '(' then
               Match_Parent := Match_Parent + 1;
            elsif Line (Curs1) = ')' then
               Match_Parent := Match_Parent - 1;
               exit when Match_Parent = 0;
            end if;

            Curs1 := Curs1 + 1;
         end loop;

         --  Skip " at "

         Curs1 := Curs1 + 5;

         if Curs1 >= Last then

            --  Maybe the file reference is on one of the next lines

            Read : loop
               Get_Line (FT, Line, Last);

               --  If we have another Frame or if the backtrace is finished
               --  the file reference was just missing

               if Last <= 1 or else Line (1) = '#' then
                  No_File := True;
                  Curs2 := Curs1 - 1;
                  return;

               else
                  Curs1 := 1;
                  while Curs1 <= Last - 2 loop
                     if Line (Curs1) = '(' then
                        Match_Parent := Match_Parent + 1;
                     elsif Line (Curs1) = ')' then
                        Match_Parent := Match_Parent - 1;
                     end if;

                     if Match_Parent = 0
                       and then Line (Curs1 .. Curs1 + 1) = "at"
                     then
                        Curs1 := Curs1 + 3;
                        exit Read;
                     end if;

                     Curs1 := Curs1 + 1;
                  end loop;
               end if;
            end loop Read;
         end if;

         --  Let's assume that the filename length is greater than 1
         --  it simplifies dealing with the potential drive ':' on
         --  windows systems

         Curs2 := Curs1 + 1;
         while Line (Curs2 + 1) /= ':' loop Curs2 := Curs2 + 1; end loop;
      end Find_File;

      ---------------
      -- Find_Line --
      ---------------

      procedure Find_Line is
      begin
         Curs1 := Curs2 + 2;
         Curs2 := Last;
         if Curs2 - Curs1 > 5 then
            raise Constraint_Error;
         end if;
      end Find_Line;

      ---------------
      -- Find_Name --
      ---------------

      procedure Find_Name is
      begin
         Curs1 := 3;

         --  Skip Frame #

         while Line (Curs1) /= ' ' loop Curs1 := Curs1 + 1; end loop;

         --  Skip spaces

         while Line (Curs1)  = ' ' loop Curs1 := Curs1 + 1; end loop;

         Curs2 := Curs1;
         while Line (Curs2 + 1) /= ' ' loop Curs2 := Curs2 + 1; end loop;
      end Find_Name;

      ------------------------
      -- Gmem_Read_BT_Frame --
      ------------------------

      procedure Gmem_Read_BT_Frame (Buf : out String; Last : out Natural) is
         procedure Read_BT_Frame (buf : System.Address);
         pragma Import (C, Read_BT_Frame, "__gnat_gmem_read_bt_frame");

         function Strlen (chars : System.Address) return Natural;
         pragma Import (C, Strlen, "strlen");

         S :  String (1 .. 1000);
      begin
         Read_BT_Frame (S'Address);
         Last := Strlen (S'Address);
         Buf (1 .. Last) := S (1 .. Last);
      end Gmem_Read_BT_Frame;

   --  Start of processing for Read_BT

   begin

      if Gmem_Mode then
         Gmem_Read_BT_Frame (Line, Last);
      else
         Line (1) := ' ';
         while Line (1) /= '#' loop
               Get_Line (FT, Line, Last);
         end loop;
      end if;

      while Last >= 1 and then Line (1) = '#' and then not Main_Found loop
         if F <= BT_Depth then
            Find_Name;
            Nam := Enter_Name (Line (Curs1 .. Curs2));
            Main_Found := Line (Curs1 .. Curs2) = "main";

            Find_File;

            if No_File then
               Fil := No_Name_Id;
               Lin := No_Name_Id;
            else
               Fil := Enter_Name (Line (Curs1 .. Curs2));

               Find_Line;
               Lin := Enter_Name (Line (Curs1 .. Curs2));
            end if;

            Frames (F) := Enter_Frame (Nam, Fil, Lin);
            F := F + 1;
         end if;

         if No_File then

            --  If no file reference was found, the next line has already
            --  been read because, it may sometimes be found on the next
            --  line

            No_File := False;

         else
            if Gmem_Mode then
               Gmem_Read_BT_Frame (Line, Last);
            else
               Get_Line (FT, Line, Last);
               exit when End_Of_File (FT);
            end if;
         end if;

      end loop;

      return Enter_Root (Frames (1 .. F - 1));
   end Read_BT;

   -------------
   -- Root_Eq --
   -------------

   function Root_Eq (N1, N2 : Root) return Boolean is
      use type Frames_In_Root.Table_Type;

   begin
      return
        Frames_In_Root.Table (N1.First .. N1.Last)
          = Frames_In_Root.Table (N2.First .. N2.Last);
   end Root_Eq;

   --------------------
   -- Set_Alloc_Size --
   --------------------

   procedure Set_Alloc_Size (B : Root_Id; V : Storage_Count) is
   begin
      Roots.Table (B).Alloc_Size := V;
   end Set_Alloc_Size;

   -------------------------
   -- Set_High_Water_Mark --
   -------------------------

   procedure Set_High_Water_Mark (B : Root_Id; V : Storage_Count) is
   begin
      Roots.Table (B).High_Water_Mark := V;
   end Set_High_Water_Mark;

   ------------------
   -- Set_Nb_Alloc --
   ------------------

   procedure Set_Nb_Alloc (B : Root_Id; V : Integer) is
   begin
      Roots.Table (B).Nb_Alloc := V;
   end Set_Nb_Alloc;

begin
   --  Initialize name for No_Name_ID

   Names.Increment_Last;
   Names.Table (Names.Last) := Name'(1, 0);
end Memroot;
