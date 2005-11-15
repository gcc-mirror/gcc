------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ M A P S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2005, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;  use Atree;
with Einfo;  use Einfo;
with Namet;  use Namet;
with Output; use Output;
with Sinfo;  use Sinfo;
with Uintp;  use Uintp;

package body Sem_Maps is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Find_Assoc (M : Map; E : Entity_Id) return Assoc_Index;
   --  Standard hash table search. M is the map to be searched, E is the
   --  entity to be searched for, and Assoc_Index is the resulting
   --  association, or is set to No_Assoc if there is no association.

   function Find_Header_Size (N : Int) return Header_Index;
   --  Find largest power of two smaller than the number of entries in
   --  the table. This load factor of 2 may be adjusted later if needed.

   procedure Write_Map (E : Entity_Id);
   pragma Warnings (Off, Write_Map);
   --  For debugging purposes

   ---------------------
   -- Add_Association --
   ---------------------

   procedure Add_Association
     (M    : in out Map;
      O_Id : Entity_Id;
      N_Id : Entity_Id;
      Kind : Scope_Kind := S_Local)
   is
      Info : constant Map_Info      := Maps_Table.Table (M);
      Offh : constant Header_Index  := Info.Header_Offset;
      Offs : constant Header_Index  := Info.Header_Num;
      J    : constant Header_Index  := Header_Index (O_Id) mod Offs;
      K    : constant Assoc_Index   := Info.Assoc_Next;

   begin
      Associations_Table.Table (K) := (O_Id, N_Id, Kind, No_Assoc);
      Maps_Table.Table (M).Assoc_Next := K + 1;

      if Headers_Table.Table (Offh + J) /= No_Assoc then

         --  Place new association at head of chain

         Associations_Table.Table (K).Next := Headers_Table.Table (Offh + J);
      end if;

      Headers_Table.Table (Offh + J) := K;
   end Add_Association;

   ------------------------
   -- Build_Instance_Map --
   ------------------------

   function Build_Instance_Map (M : Map) return Map is
      Info    : constant Map_Info     := Maps_Table.Table (M);
      Res     : constant Map          := New_Map (Int (Info.Assoc_Num));
      Offh1   : constant Header_Index := Info.Header_Offset;
      Offa1   : constant Assoc_Index  := Info.Assoc_Offset;
      Offh2   : constant Header_Index := Maps_Table.Table (Res).Header_Offset;
      Offa2   : constant Assoc_Index  := Maps_Table.Table (Res).Assoc_Offset;
      A       : Assoc;
      A_Index : Assoc_Index;

   begin
      for J in 0 .. Info.Header_Num - 1 loop
         A_Index := Headers_Table.Table (Offh1 + J);

         if A_Index /= No_Assoc then
            Headers_Table.Table (Offh2 + J) := A_Index + (Offa2 - Offa1);
         end if;
      end loop;

      for J in 0 .. Info.Assoc_Num - 1 loop
         A  := Associations_Table.Table (Offa1 + J);

         --  For local entities that come from source, create the
         --  corresponding local entities in the instance. Entities that
         --  do not come from source are etypes, and new ones will be
         --  generated when analyzing the instance.

         if No (A.New_Id)
           and then A.Kind = S_Local
           and then Comes_From_Source (A.Old_Id)
         then
            A.New_Id := New_Copy (A.Old_Id);
            A.New_Id := New_Entity (Nkind (A.Old_Id), Sloc (A.Old_Id));
            Set_Chars (A.New_Id, Chars (A.Old_Id));
         end if;

         if A.Next /= No_Assoc then
            A.Next := A.Next + (Offa2 - Offa1);
         end if;

         Associations_Table.Table (Offa2 + J) := A;
      end loop;

      Maps_Table.Table (Res).Assoc_Next := Associations_Table.Last;
      return Res;
   end Build_Instance_Map;

   -------------
   -- Compose --
   -------------

   function Compose (Orig_Map : Map; New_Map : Map) return Map is
      Res : constant Map         := Copy (Orig_Map);
      Off : constant Assoc_Index := Maps_Table.Table (Res).Assoc_Offset;
      A   : Assoc;
      K   : Assoc_Index;

   begin
      --  Iterate over the contents of Orig_Map, looking for entities
      --  that are further mapped under New_Map.

      for J in 0 .. Maps_Table.Table (Res).Assoc_Num - 1  loop
         A := Associations_Table.Table (Off + J);
         K := Find_Assoc (New_Map, A.New_Id);

         if K /= No_Assoc then
            Associations_Table.Table (Off + J).New_Id
              := Associations_Table.Table (K).New_Id;
         end if;
      end loop;

      return Res;
   end Compose;

   ----------
   -- Copy --
   ----------

   function Copy (M : Map) return Map is
      Info    : constant Map_Info     := Maps_Table.Table (M);
      Res     : constant Map          := New_Map (Int (Info.Assoc_Num));
      Offh1   : constant Header_Index := Info.Header_Offset;
      Offa1   : constant Assoc_Index  := Info.Assoc_Offset;
      Offh2   : constant Header_Index := Maps_Table.Table (Res).Header_Offset;
      Offa2   : constant Assoc_Index  := Maps_Table.Table (Res).Assoc_Offset;
      A       : Assoc;
      A_Index : Assoc_Index;

   begin
      for J in 0 .. Info.Header_Num - 1 loop
         A_Index := Headers_Table.Table (Offh1 + J) + (Offa2 - Offa1);

         if A_Index /= No_Assoc then
            Headers_Table.Table (Offh2 + J) := A_Index + (Offa2 - Offa1);
         end if;
      end loop;

      for J in 0 .. Info.Assoc_Num - 1 loop
         A := Associations_Table.Table (Offa1 + J);
         A.Next := A.Next + (Offa2 - Offa1);
         Associations_Table.Table (Offa2 + J) := A;
      end loop;

      Maps_Table.Table (Res).Assoc_Next := Associations_Table.Last;
      return Res;
   end Copy;

   ----------------
   -- Find_Assoc --
   ----------------

   function Find_Assoc (M : Map; E : Entity_Id) return Assoc_Index is
      Offh    : constant Header_Index := Maps_Table.Table (M).Header_Offset;
      Offs    : constant Header_Index := Maps_Table.Table (M).Header_Num;
      J       : constant Header_Index := Header_Index (E) mod Offs;
      A       : Assoc;
      A_Index : Assoc_Index;

   begin
      A_Index := Headers_Table.Table (Offh + J);

      if A_Index = No_Assoc then
         return A_Index;

      else
         A := Associations_Table.Table (A_Index);

         while Present (A.Old_Id) loop

            if A.Old_Id = E then
               return A_Index;

            elsif A.Next = No_Assoc then
               return No_Assoc;

            else
               A_Index := A.Next;
               A := Associations_Table.Table (A.Next);
            end if;
         end loop;

         return No_Assoc;
      end if;
   end Find_Assoc;

   ----------------------
   -- Find_Header_Size --
   ----------------------

   function Find_Header_Size (N : Int) return Header_Index is
      Siz : Header_Index;

   begin
      Siz := 2;
      while 2 * Siz < Header_Index (N) loop
         Siz := 2 * Siz;
      end loop;

      return Siz;
   end Find_Header_Size;

   ------------
   -- Lookup --
   ------------

   function Lookup (M : Map; E : Entity_Id) return Entity_Id is
      Offh : constant Header_Index := Maps_Table.Table (M).Header_Offset;
      Offs : constant Header_Index := Maps_Table.Table (M).Header_Num;
      J    : constant Header_Index := Header_Index (E) mod Offs;
      A    : Assoc;

   begin
      if Headers_Table.Table (Offh + J) = No_Assoc then
         return Empty;

      else
         A := Associations_Table.Table (Headers_Table.Table (Offh + J));

         while Present (A.Old_Id) loop

            if A.Old_Id = E then
               return A.New_Id;

            elsif A.Next = No_Assoc then
               return Empty;

            else
               A := Associations_Table.Table (A.Next);
            end if;
         end loop;

         return Empty;
      end if;
   end Lookup;

   -------------
   -- New_Map --
   -------------

   function New_Map (Num_Assoc : Int) return Map is
      Header_Size : constant Header_Index := Find_Header_Size (Num_Assoc);
      Res         : Map_Info;

   begin
      --  Allocate the tables for the new map at the current end of the
      --  global tables.

      Associations_Table.Increment_Last;
      Headers_Table.Increment_Last;
      Maps_Table.Increment_Last;

      Res.Header_Offset := Headers_Table.Last;
      Res.Header_Num    := Header_Size;
      Res.Assoc_Offset  := Associations_Table.Last;
      Res.Assoc_Next    := Associations_Table.Last;
      Res.Assoc_Num     := Assoc_Index (Num_Assoc);

      Headers_Table.Set_Last (Headers_Table.Last + Header_Size);
      Associations_Table.Set_Last
        (Associations_Table.Last + Assoc_Index (Num_Assoc));
      Maps_Table.Table (Maps_Table.Last) := Res;

      for J in 1 .. Header_Size loop
         Headers_Table.Table (Headers_Table.Last - J) := No_Assoc;
      end loop;

      return Maps_Table.Last;
   end New_Map;

   ------------------------
   -- Update_Association --
   ------------------------

   procedure Update_Association
     (M    : in out Map;
      O_Id : Entity_Id;
      N_Id : Entity_Id;
      Kind : Scope_Kind := S_Local)
   is
      J : constant Assoc_Index := Find_Assoc (M, O_Id);

   begin
      Associations_Table.Table (J).New_Id := N_Id;
      Associations_Table.Table (J).Kind := Kind;
   end Update_Association;

   ---------------
   -- Write_Map --
   ---------------

   procedure Write_Map (E : Entity_Id) is
      M    : constant Map          := Map (UI_To_Int (Renaming_Map (E)));
      Info : constant Map_Info     := Maps_Table.Table (M);
      Offh : constant Header_Index := Info.Header_Offset;
      Offa : constant Assoc_Index  := Info.Assoc_Offset;
      A    : Assoc;

   begin
      Write_Str ("Size : ");
      Write_Int (Int (Info.Assoc_Num));
      Write_Eol;

      Write_Str ("Headers");
      Write_Eol;

      for J in 0 .. Info.Header_Num - 1 loop
         Write_Int (Int (Offh + J));
         Write_Str (" : ");
         Write_Int (Int (Headers_Table.Table (Offh + J)));
         Write_Eol;
      end loop;

      for J in 0 .. Info.Assoc_Num - 1 loop
         A := Associations_Table.Table (Offa + J);
         Write_Int (Int (Offa + J));
         Write_Str (" : ");
         Write_Name (Chars (A.Old_Id));
         Write_Str ("  ");
         Write_Int (Int (A.Old_Id));
         Write_Str (" ==> ");
         Write_Int (Int (A.New_Id));
         Write_Str (" next = ");
         Write_Int (Int (A.Next));
         Write_Eol;
      end loop;
   end Write_Map;

end Sem_Maps;
