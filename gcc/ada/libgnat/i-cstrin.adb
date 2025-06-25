------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C . S T R I N G S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

package body Interfaces.C.Strings with
  SPARK_Mode => Off
is

   --  Note that the type chars_ptr has a pragma No_Strict_Aliasing in the
   --  spec, to prevent any assumptions about aliasing for values of this type,
   --  since arbitrary addresses can be converted, and it is quite likely that
   --  this type will in fact be used for aliasing values of other types.

   --  Convert between chars_ptr and a C pointer
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

   procedure Position_Of_Nul
     (Into : char_array; Found : out Boolean; Index : out size_t);
   --  If into contains a Nul character, Found is set to True and Index
   --  contains the position of the first Nul character in Into. Otherwise
   --  Found is set to False and the value of Index is not meaningful.

   --  We can't use directly System.Memory because the categorization is not
   --  compatible, so we directly import here the malloc and free routines.

   function Memory_Alloc (Size : size_t) return chars_ptr;
   pragma Import (C, Memory_Alloc, System.Parameters.C_Malloc_Linkname);
   --  Allocate a chunk of memory on the heap

   procedure Memory_Free (Address : chars_ptr);
   pragma Import (C, Memory_Free, "__gnat_free");
   --  Deallocate a previously allocated chunk of memory from the heap. On
   --  runtimes that do not allow deallocation this is a no-op.

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
      if Item /= Null_Ptr then
         Memory_Free (Item);
         Item := Null_Ptr;
      end if;
   end Free;

   --------------------
   -- New_Char_Array --
   --------------------

   function New_Char_Array (Chars : char_array) return chars_ptr is
      Found   : Boolean;
      Index   : size_t;
      Pointer : chars_ptr;

   begin
      --  Get index of position of null. If Index > Chars'Last,
      --  nul is absent and must be added explicitly.

      Position_Of_Nul (Into => Chars, Found => Found, Index => Index);

      --  If nul is present, transfer string up to and including nul

      if Found then
         Pointer := Memory_Alloc (Index - Chars'First + 1);

         Update
           (Item   => Pointer,
            Offset => 0,
            Chars  => Chars (Chars'First .. Index),
            Check  => False);
      else
         --  If original string has no nul, transfer whole string and add
         --  terminator explicitly.

         Pointer := Memory_Alloc (Chars'Length + 1);

         Update (Item => Pointer, Offset => 0, Chars => Chars, Check => False);
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

      Len : Natural := 0;
      --  Length of the longest prefix of Str that doesn't contain NUL

      Result : chars_ptr;
   begin
      for C of Str loop
         if C = ASCII.NUL then
            exit;
         end if;
         Len := Len + 1;
      end loop;

      Result := Memory_Alloc (size_t (Len) + 1);

      declare
         Result_Array : char_array (1 .. size_t (Len) + 1)
         with Address => To_Address (Result), Import, Convention => Ada;

         Count : size_t;
      begin
         To_C
           (Item       => Str (Str'First .. Str'First + Len - 1),
            Target     => Result_Array,
            Count      => Count,
            Append_Nul => True);
      end;

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

   procedure Position_Of_Nul
     (Into : char_array; Found : out Boolean; Index : out size_t) is
   begin
      Found := False;
      Index := 0;

      for J in Into'Range loop
         if Into (J) = nul then
            Found := True;
            Index := J;
            return;
         end if;
      end loop;
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
      Found : Boolean;
      Index : size_t;
   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "early returns for performance");
      if Item = null then
         return Null_Ptr;
      elsif Nul_Check then
         Position_Of_Nul (Item.all, Found, Index);
         if not Found then
            raise Terminator_Error;
         end if;
      end if;

      return To_chars_ptr (Item (Item'First)'Address);

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
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
      --  Check for null pointer as mandated by the RM.
      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

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
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "early returns for performance");
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

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Value;

   function Value (Item : chars_ptr) return String is
   begin
      return To_Ada (Value (Item));
   end Value;

   function Value (Item : chars_ptr; Length : size_t) return String is
      Result : String (1 .. Natural (Length));
      C : char;

   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "early returns for performance");

      --  As per AI-00177, this is equivalent to:

      --    To_Ada (Value (Item, Length) & nul);

      if Item = Null_Ptr then
         raise Dereference_Error;
      end if;

      for J in Result'Range loop
         C := Peek (Item + size_t (J - 1));

         if C = nul then
            return Result (1 .. J - 1);
         else
            Result (J) := To_Ada (C);
         end if;
      end loop;

      return Result;

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Value;

end Interfaces.C.Strings;
