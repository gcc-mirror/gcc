------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                         G N A T . S P I T B O L                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2017, AdaCore                     --
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

with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;

with GNAT.Debug_Utilities;      use GNAT.Debug_Utilities;
with GNAT.IO;                   use GNAT.IO;

with System.String_Hash;

with Ada.Unchecked_Deallocation;

package body GNAT.Spitbol is

   ---------
   -- "&" --
   ---------

   function "&" (Num : Integer; Str : String)  return String is
   begin
      return S (Num) & Str;
   end "&";

   function "&" (Str : String;  Num : Integer) return String is
   begin
      return Str & S (Num);
   end "&";

   function "&" (Num : Integer; Str : VString) return VString is
   begin
      return S (Num) & Str;
   end "&";

   function "&" (Str : VString; Num : Integer) return VString is
   begin
      return Str & S (Num);
   end "&";

   ----------
   -- Char --
   ----------

   function Char (Num : Natural) return Character is
   begin
      return Character'Val (Num);
   end Char;

   ----------
   -- Lpad --
   ----------

   function Lpad
     (Str : VString;
      Len : Natural;
      Pad : Character := ' ') return VString
   is
   begin
      if Length (Str) >= Len then
         return Str;
      else
         return Tail (Str, Len, Pad);
      end if;
   end Lpad;

   function Lpad
     (Str : String;
      Len : Natural;
      Pad : Character := ' ') return VString
   is
   begin
      if Str'Length >= Len then
         return V (Str);

      else
         declare
            R : String (1 .. Len);

         begin
            for J in 1 .. Len - Str'Length loop
               R (J) := Pad;
            end loop;

            R (Len - Str'Length + 1 .. Len) := Str;
            return V (R);
         end;
      end if;
   end Lpad;

   procedure Lpad
     (Str  : in out VString;
      Len  : Natural;
      Pad  : Character := ' ')
   is
   begin
      if Length (Str) >= Len then
         return;
      else
         Tail (Str, Len, Pad);
      end if;
   end Lpad;

   -------
   -- N --
   -------

   function N (Str : VString) return Integer is
      S : Big_String_Access;
      L : Natural;
   begin
      Get_String (Str, S, L);
      return Integer'Value (S (1 .. L));
   end N;

   --------------------
   -- Reverse_String --
   --------------------

   function Reverse_String (Str : VString) return VString is
      S : Big_String_Access;
      L : Natural;

   begin
      Get_String (Str, S, L);

      declare
         Result : String (1 .. L);

      begin
         for J in 1 .. L loop
            Result (J) := S (L + 1 - J);
         end loop;

         return V (Result);
      end;
   end Reverse_String;

   function Reverse_String (Str : String) return VString is
      Result : String (1 .. Str'Length);

   begin
      for J in 1 .. Str'Length loop
         Result (J) := Str (Str'Last + 1 - J);
      end loop;

      return V (Result);
   end Reverse_String;

   procedure Reverse_String (Str : in out VString) is
      S : Big_String_Access;
      L : Natural;

   begin
      Get_String (Str, S, L);

      declare
         Result : String (1 .. L);

      begin
         for J in 1 .. L loop
            Result (J) := S (L + 1 - J);
         end loop;

         Set_Unbounded_String (Str, Result);
      end;
   end Reverse_String;

   ----------
   -- Rpad --
   ----------

   function Rpad
     (Str : VString;
      Len : Natural;
      Pad : Character := ' ') return VString
   is
   begin
      if Length (Str) >= Len then
         return Str;
      else
         return Head (Str, Len, Pad);
      end if;
   end Rpad;

   function Rpad
     (Str : String;
      Len : Natural;
      Pad : Character := ' ') return VString
   is
   begin
      if Str'Length >= Len then
         return V (Str);

      else
         declare
            R : String (1 .. Len);

         begin
            for J in Str'Length + 1 .. Len loop
               R (J) := Pad;
            end loop;

            R (1 .. Str'Length) := Str;
            return V (R);
         end;
      end if;
   end Rpad;

   procedure Rpad
     (Str  : in out VString;
      Len  : Natural;
      Pad  : Character := ' ')
   is
   begin
      if Length (Str) >= Len then
         return;

      else
         Head (Str, Len, Pad);
      end if;
   end Rpad;

   -------
   -- S --
   -------

   function S (Num : Integer) return String is
      Buf : String (1 .. 30);
      Ptr : Natural := Buf'Last + 1;
      Val : Natural := abs (Num);

   begin
      loop
         Ptr := Ptr - 1;
         Buf (Ptr) := Character'Val (Val mod 10 + Character'Pos ('0'));
         Val := Val / 10;
         exit when Val = 0;
      end loop;

      if Num < 0 then
         Ptr := Ptr - 1;
         Buf (Ptr) := '-';
      end if;

      return Buf (Ptr .. Buf'Last);
   end S;

   ------------
   -- Substr --
   ------------

   function Substr
     (Str   : VString;
      Start : Positive;
      Len   : Natural) return VString
   is
      S : Big_String_Access;
      L : Natural;

   begin
      Get_String (Str, S, L);

      if Start > L then
         raise Index_Error;
      elsif Start + Len - 1 > L then
         raise Length_Error;
      else
         return V (S (Start .. Start + Len - 1));
      end if;
   end Substr;

   function Substr
     (Str   : String;
      Start : Positive;
      Len   : Natural) return VString
   is
   begin
      if Start > Str'Length then
         raise Index_Error;
      elsif Start + Len - 1 > Str'Length then
         raise Length_Error;
      else
         return
           V (Str (Str'First + Start - 1 .. Str'First + Start + Len - 2));
      end if;
   end Substr;

   -----------
   -- Table --
   -----------

   package body Table is

      procedure Free is new
        Ada.Unchecked_Deallocation (Hash_Element, Hash_Element_Ptr);

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Hash is new System.String_Hash.Hash
        (Character, String, Unsigned_32);

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Object : in out Table) is
         Ptr1 : Hash_Element_Ptr;
         Ptr2 : Hash_Element_Ptr;

      begin
         for J in Object.Elmts'Range loop
            Ptr1 := Object.Elmts (J)'Unrestricted_Access;

            if Ptr1.Name /= null then
               loop
                  Ptr1.Name := new String'(Ptr1.Name.all);
                  exit when Ptr1.Next = null;
                  Ptr2 := Ptr1.Next;
                  Ptr1.Next := new Hash_Element'(Ptr2.all);
                  Ptr1 := Ptr1.Next;
               end loop;
            end if;
         end loop;
      end Adjust;

      -----------
      -- Clear --
      -----------

      procedure Clear (T : in out Table) is
         Ptr1 : Hash_Element_Ptr;
         Ptr2 : Hash_Element_Ptr;

      begin
         for J in T.Elmts'Range loop
            if T.Elmts (J).Name /= null then
               Free (T.Elmts (J).Name);
               T.Elmts (J).Value := Null_Value;

               Ptr1 := T.Elmts (J).Next;
               T.Elmts (J).Next := null;

               while Ptr1 /= null loop
                  Ptr2 := Ptr1.Next;
                  Free (Ptr1.Name);
                  Free (Ptr1);
                  Ptr1 := Ptr2;
               end loop;
            end if;
         end loop;
      end Clear;

      ----------------------
      -- Convert_To_Array --
      ----------------------

      function Convert_To_Array (T : Table) return Table_Array is
         Num_Elmts : Natural := 0;
         Elmt      : Hash_Element_Ptr;

      begin
         for J in T.Elmts'Range loop
            Elmt := T.Elmts (J)'Unrestricted_Access;

            if Elmt.Name /= null then
               loop
                  Num_Elmts := Num_Elmts + 1;
                  Elmt := Elmt.Next;
                  exit when Elmt = null;
               end loop;
            end if;
         end loop;

         declare
            TA  : Table_Array (1 .. Num_Elmts);
            P   : Natural := 1;

         begin
            for J in T.Elmts'Range loop
               Elmt := T.Elmts (J)'Unrestricted_Access;

               if Elmt.Name /= null then
                  loop
                     Set_Unbounded_String (TA (P).Name, Elmt.Name.all);
                     TA (P).Value := Elmt.Value;
                     P := P + 1;
                     Elmt := Elmt.Next;
                     exit when Elmt = null;
                  end loop;
               end if;
            end loop;

            return TA;
         end;
      end Convert_To_Array;

      ----------
      -- Copy --
      ----------

      procedure Copy (From : Table; To : in out Table) is
         Elmt : Hash_Element_Ptr;

      begin
         Clear (To);

         for J in From.Elmts'Range loop
            Elmt := From.Elmts (J)'Unrestricted_Access;
            if Elmt.Name /= null then
               loop
                  Set (To, Elmt.Name.all, Elmt.Value);
                  Elmt := Elmt.Next;
                  exit when Elmt = null;
               end loop;
            end if;
         end loop;
      end Copy;

      ------------
      -- Delete --
      ------------

      procedure Delete (T : in out Table; Name : Character) is
      begin
         Delete (T, String'(1 => Name));
      end Delete;

      procedure Delete (T : in out Table; Name  : VString) is
         S : Big_String_Access;
         L : Natural;
      begin
         Get_String (Name, S, L);
         Delete (T, S (1 .. L));
      end Delete;

      procedure Delete (T : in out Table; Name  : String) is
         Slot : constant Unsigned_32 := Hash (Name) mod T.N + 1;
         Elmt : Hash_Element_Ptr := T.Elmts (Slot)'Unrestricted_Access;
         Next : Hash_Element_Ptr;

      begin
         if Elmt.Name = null then
            null;

         elsif Elmt.Name.all = Name then
            Free (Elmt.Name);

            if Elmt.Next = null then
               Elmt.Value := Null_Value;
               return;

            else
               Next := Elmt.Next;
               Elmt.Name  := Next.Name;
               Elmt.Value := Next.Value;
               Elmt.Next  := Next.Next;
               Free (Next);
               return;
            end if;

         else
            loop
               Next := Elmt.Next;

               if Next = null then
                  return;

               elsif Next.Name.all = Name then
                  Free (Next.Name);
                  Elmt.Next := Next.Next;
                  Free (Next);
                  return;

               else
                  Elmt := Next;
               end if;
            end loop;
         end if;
      end Delete;

      ----------
      -- Dump --
      ----------

      procedure Dump (T : Table; Str : String := "Table") is
         Num_Elmts : Natural := 0;
         Elmt      : Hash_Element_Ptr;

      begin
         for J in T.Elmts'Range loop
            Elmt := T.Elmts (J)'Unrestricted_Access;

            if Elmt.Name /= null then
               loop
                  Num_Elmts := Num_Elmts + 1;
                  Put_Line
                    (Str & '<' & Image (Elmt.Name.all) & "> = " &
                     Img (Elmt.Value));
                  Elmt := Elmt.Next;
                  exit when Elmt = null;
               end loop;
            end if;
         end loop;

         if Num_Elmts = 0 then
            Put_Line (Str & " is empty");
         end if;
      end Dump;

      procedure Dump (T : Table_Array; Str : String := "Table_Array") is
      begin
         if T'Length = 0 then
            Put_Line (Str & " is empty");

         else
            for J in T'Range loop
               Put_Line
                 (Str & '(' & Image (To_String (T (J).Name)) & ") = " &
                  Img (T (J).Value));
            end loop;
         end if;
      end Dump;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Object : in out Table) is
         Ptr1 : Hash_Element_Ptr;
         Ptr2 : Hash_Element_Ptr;

      begin
         for J in Object.Elmts'Range loop
            Ptr1 := Object.Elmts (J).Next;
            Free (Object.Elmts (J).Name);
            while Ptr1 /= null loop
               Ptr2 := Ptr1.Next;
               Free (Ptr1.Name);
               Free (Ptr1);
               Ptr1 := Ptr2;
            end loop;
         end loop;
      end Finalize;

      ---------
      -- Get --
      ---------

      function Get (T : Table; Name : Character) return Value_Type is
      begin
         return Get (T, String'(1 => Name));
      end Get;

      function Get (T : Table; Name : VString) return Value_Type is
         S : Big_String_Access;
         L : Natural;
      begin
         Get_String (Name, S, L);
         return Get (T, S (1 .. L));
      end Get;

      function Get (T : Table; Name : String) return Value_Type is
         Slot : constant Unsigned_32 := Hash (Name) mod T.N + 1;
         Elmt : Hash_Element_Ptr := T.Elmts (Slot)'Unrestricted_Access;

      begin
         if Elmt.Name = null then
            return Null_Value;

         else
            loop
               if Name = Elmt.Name.all then
                  return Elmt.Value;

               else
                  Elmt := Elmt.Next;

                  if Elmt = null then
                     return Null_Value;
                  end if;
               end if;
            end loop;
         end if;
      end Get;

      -------------
      -- Present --
      -------------

      function Present (T : Table; Name : Character) return Boolean is
      begin
         return Present (T, String'(1 => Name));
      end Present;

      function Present (T : Table; Name : VString) return Boolean is
         S : Big_String_Access;
         L : Natural;
      begin
         Get_String (Name, S, L);
         return Present (T, S (1 .. L));
      end Present;

      function Present (T : Table; Name : String) return Boolean is
         Slot : constant Unsigned_32 := Hash (Name) mod T.N + 1;
         Elmt : Hash_Element_Ptr := T.Elmts (Slot)'Unrestricted_Access;

      begin
         if Elmt.Name = null then
            return False;

         else
            loop
               if Name = Elmt.Name.all then
                  return True;

               else
                  Elmt := Elmt.Next;

                  if Elmt = null then
                     return False;
                  end if;
               end if;
            end loop;
         end if;
      end Present;

      ---------
      -- Set --
      ---------

      procedure Set (T : in out Table; Name : VString; Value : Value_Type) is
         S : Big_String_Access;
         L : Natural;
      begin
         Get_String (Name, S, L);
         Set (T, S (1 .. L), Value);
      end Set;

      procedure Set (T : in out Table; Name : Character; Value : Value_Type) is
      begin
         Set (T, String'(1 => Name), Value);
      end Set;

      procedure Set
        (T     : in out Table;
         Name  : String;
         Value : Value_Type)
      is
      begin
         if Value = Null_Value then
            Delete (T, Name);

         else
            declare
               Slot : constant Unsigned_32 := Hash (Name) mod T.N + 1;
               Elmt : Hash_Element_Ptr := T.Elmts (Slot)'Unrestricted_Access;

               subtype String1 is String (1 .. Name'Length);

            begin
               if Elmt.Name = null then
                  Elmt.Name  := new String'(String1 (Name));
                  Elmt.Value := Value;
                  return;

               else
                  loop
                     if Name = Elmt.Name.all then
                        Elmt.Value := Value;
                        return;

                     elsif Elmt.Next = null then
                        Elmt.Next := new Hash_Element'(
                                       Name  => new String'(String1 (Name)),
                                       Value => Value,
                                       Next  => null);
                        return;

                     else
                        Elmt := Elmt.Next;
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end Set;
   end Table;

   ----------
   -- Trim --
   ----------

   function Trim (Str : VString) return VString is
   begin
      return Trim (Str, Right);
   end Trim;

   function Trim (Str : String) return VString is
   begin
      for J in reverse Str'Range loop
         if Str (J) /= ' ' then
            return V (Str (Str'First .. J));
         end if;
      end loop;

      return Nul;
   end Trim;

   procedure Trim (Str : in out VString) is
   begin
      Trim (Str, Right);
   end Trim;

   -------
   -- V --
   -------

   function V (Num : Integer) return VString is
      Buf : String (1 .. 30);
      Ptr : Natural := Buf'Last + 1;
      Val : Natural := abs (Num);

   begin
      loop
         Ptr := Ptr - 1;
         Buf (Ptr) := Character'Val (Val mod 10 + Character'Pos ('0'));
         Val := Val / 10;
         exit when Val = 0;
      end loop;

      if Num < 0 then
         Ptr := Ptr - 1;
         Buf (Ptr) := '-';
      end if;

      return V (Buf (Ptr .. Buf'Last));
   end V;

end GNAT.Spitbol;
