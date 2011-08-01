------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C . S T R I N G S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Ada.Unchecked_Conversion;

package body Interfaces.C.Strings is

   --  Note that the type chars_ptr has a pragma No_Strict_Aliasing in the
   --  spec, to prevent any assumptions about aliasing for values of this type,
   --  since arbitrary addresses can be converted, and it is quite likely that
   --  this type will in fact be used for aliasing values of other types.

   function To_chars_ptr is
      new Ada.Unchecked_Conversion (System.Parameters.C_Address, chars_ptr);

   function To_Address is
      new Ada.Unchecked_Conversion (chars_ptr, System.Parameters.C_Address);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Peek (From : chars_ptr) return char;
   pragma Inline (Peek);
   --  Given a chars_ptr value, obtain referenced character

   procedure Poke (Value : char; Into : chars_ptr);
   pragma Inline (Poke);
   --  Given a chars_ptr, modify referenced Character value

   function "+" (Left : chars_ptr; Right : size_t) return chars_ptr;
   pragma Inline ("+");
   --  Address arithmetic on chars_ptr value

   function Position_Of_Nul (Into : char_array) return size_t;
   --  Returns position of the first Nul in Into or Into'Last + 1 if none

   --  We can't use directly System.Memory because the categorization is not
   --  compatible, so we directly import here the malloc and free routines.

   function Memory_Alloc (Size : size_t) return chars_ptr;
   pragma Import (C, Memory_Alloc, System.Parameters.C_Malloc_Linkname);

   procedure Memory_Free (Address : chars_ptr);
   pragma Import (C, Memory_Free, "__gnat_free");

   ---------
   -- "+" --
   ---------

   function "+" (Left : chars_ptr; Right : size_t) return chars_ptr is
   begin
      return To_chars_ptr (To_Address (Left) + Storage_Offset (Right));
   end "+";

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out chars_ptr) is
   begin
      if Item = Null_Ptr then
         return;
      end if;

      Memory_Free (Item);
      Item := Null_Ptr;
   end Free;

   --------------------
   -- New_Char_Array --
   --------------------

   function New_Char_Array (Chars : char_array) return chars_ptr is
      Index   : size_t;
      Pointer : chars_ptr;

   begin
      --  Get index of position of null. If Index > Chars'Last,
      --  nul is absent and must be added explicitly.

      Index := Position_Of_Nul (Into => Chars);
      Pointer := Memory_Alloc ((Index - Chars'First + 1));

      --  If nul is present, transfer string up to and including nul

      if Index <= Chars'Last then
         Update (Item   => Pointer,
                 Offset => 0,
                 Chars  => Chars (Chars'First .. Index),
                 Check  => False);
      else
         --  If original string has no nul, transfer whole string and add
         --  terminator explicitly.

         Update (Item   => Pointer,
                 Offset => 0,
                 Chars  => Chars,
                 Check  => False);
         Poke (nul, Into => Pointer + size_t'(Chars'Length));
      end if;

      return Pointer;
   end New_Char_Array;

   ----------------
   -- New_String --
   ----------------

   function New_String (Str : String) return chars_ptr is

      --  It's important that this subprogram uses the heap directly to compute
      --  the result, and doesn't copy the string on the stack, otherwise its
      --  use is limited when used from tasks on large strings.

      Result : constant chars_ptr := Memory_Alloc (Str'Length + 1);

      Result_Array : char_array  (1 .. Str'Length + 1);
      for Result_Array'Address use To_Address (Result);
      pragma Import (Ada, Result_Array);

      Count : size_t;

   begin
      To_C
        (Item       => Str,
         Target     => Result_Array,
         Count      => Count,
         Append_Nul => True);
      return Result;
   end New_String;

   ----------
   -- Peek --
   ----------

   function Peek (From : chars_ptr) return char is
   begin
      return char (From.all);
   end Peek;

   ----------
   -- Poke --
   ----------

   procedure Poke (Value : char; Into : chars_ptr) is
   begin
      Into.all := Character (Value);
   end Poke;

   ---------------------
   -- Position_Of_Nul --
   ---------------------

   function Position_Of_Nul (Into : char_array) return size_t is
   begin
      for J in Into'Range loop
         if Into (J) = nul then
            return J;
         end if;
      end loop;

      return Into'Last + 1;
   end Position_Of_Nul;

   ------------
   -- Strlen --
   ------------

   function Strlen (Item : chars_ptr) return size_t is
      Item_Index : size_t := 0;

   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      loop
         if Peek (Item + Item_Index) = nul then
            return Item_Index;
         end if;

         Item_Index := Item_Index + 1;
      end loop;
   end Strlen;

   ------------------
   -- To_Chars_Ptr --
   ------------------

   function To_Chars_Ptr
     (Item      : char_array_access;
      Nul_Check : Boolean := False) return chars_ptr
   is
   begin
      if Item = null then
         return Null_Ptr;
      elsif Nul_Check
        and then Position_Of_Nul (Into => Item.all) > Item'Last
      then
         raise Terminator_Error;
      else
         return To_chars_ptr (Item (Item'First)'Address);
      end if;
   end To_Chars_Ptr;

   ------------
   -- Update --
   ------------

   procedure Update
     (Item   : chars_ptr;
      Offset : size_t;
      Chars  : char_array;
      Check  : Boolean := True)
   is
      Index : chars_ptr := Item + Offset;

   begin
      if Check and then Offset + Chars'Length  > Strlen (Item) then
         raise Update_Error;
      end if;

      for J in Chars'Range loop
         Poke (Chars (J), Into => Index);
         Index := Index + size_t'(1);
      end loop;
   end Update;

   procedure Update
     (Item   : chars_ptr;
      Offset : size_t;
      Str    : String;
      Check  : Boolean := True)
   is
   begin
      --  Note: in RM 95, the Append_Nul => False parameter is omitted. But
      --  this has the unintended consequence of truncating the string after
      --  an update. As discussed in Ada 2005 AI-242, this was unintended,
      --  and should be corrected. Since this is a clear error, it seems
      --  appropriate to apply the correction in Ada 95 mode as well.

      Update (Item, Offset, To_C (Str, Append_Nul => False), Check);
   end Update;

   -----------
   -- Value --
   -----------

   function Value (Item : chars_ptr) return char_array is
      Result : char_array (0 .. Strlen (Item));

   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      --  Note that the following loop will also copy the terminating Nul

      for J in Result'Range loop
         Result (J) := Peek (Item + J);
      end loop;

      return Result;
   end Value;

   function Value
     (Item   : chars_ptr;
      Length : size_t) return char_array
   is
   begin
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      --  ACATS cxb3010 checks that Constraint_Error gets raised when Length
      --  is 0. Seems better to check that Length is not null before declaring
      --  an array with size_t bounds of 0 .. Length - 1 anyway.

      if Length = 0 then
         raise Constraint_Error;
      end if;

      declare
         Result : char_array (0 .. Length - 1);

      begin
         for J in Result'Range loop
            Result (J) := Peek (Item + J);

            if Result (J) = nul then
               return Result (0 .. J);
            end if;
         end loop;

         return Result;
      end;
   end Value;

   function Value (Item : chars_ptr) return String is
   begin
      return To_Ada (Value (Item));
   end Value;

   function Value (Item : chars_ptr; Length : size_t) return String is
      Result : char_array (0 .. Length);

   begin
      --  As per AI-00177, this is equivalent to:

      --    To_Ada (Value (Item, Length) & nul);

      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      for J in 0 .. Length - 1 loop
         Result (J) := Peek (Item + J);

         if Result (J) = nul then
            return To_Ada (Result (0 .. J));
         end if;
      end loop;

      Result (Length) := nul;
      return To_Ada (Result);
   end Value;

end Interfaces.C.Strings;
