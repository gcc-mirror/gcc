------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . S T R I N G S . U N B O U N D E D                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with Ada.Strings.Fixed;
with Ada.Strings.Search;
with Ada.Unchecked_Deallocation;

package body Ada.Strings.Unbounded is

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Unbounded_String
   is
      L_Length : constant Natural := Left.Last;
      R_Length : constant Natural := Right.Last;
      Result   : Unbounded_String;

   begin
      Result.Last := L_Length + R_Length;

      Result.Reference := new String (1 .. Result.Last);

      Result.Reference (1 .. L_Length) :=
        Left.Reference (1 .. Left.Last);
      Result.Reference (L_Length + 1 .. Result.Last) :=
        Right.Reference (1 .. Right.Last);

      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_String;
      Right : String) return Unbounded_String
   is
      L_Length : constant Natural := Left.Last;
      Result   : Unbounded_String;

   begin
      Result.Last := L_Length + Right'Length;

      Result.Reference := new String (1 .. Result.Last);

      Result.Reference (1 .. L_Length) := Left.Reference (1 .. Left.Last);
      Result.Reference (L_Length + 1 .. Result.Last) := Right;

      return Result;
   end "&";

   function "&"
     (Left  : String;
      Right : Unbounded_String) return Unbounded_String
   is
      R_Length : constant Natural := Right.Last;
      Result   : Unbounded_String;

   begin
      Result.Last := Left'Length + R_Length;

      Result.Reference := new String (1 .. Result.Last);

      Result.Reference (1 .. Left'Length) := Left;
      Result.Reference (Left'Length + 1 .. Result.Last) :=
        Right.Reference (1 .. Right.Last);

      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_String;
      Right : Character) return Unbounded_String
   is
      Result : Unbounded_String;

   begin
      Result.Last := Left.Last + 1;

      Result.Reference := new String (1 .. Result.Last);

      Result.Reference (1 .. Result.Last - 1) :=
        Left.Reference (1 .. Left.Last);
      Result.Reference (Result.Last) := Right;

      return Result;
   end "&";

   function "&"
     (Left  : Character;
      Right : Unbounded_String) return Unbounded_String
   is
      Result : Unbounded_String;

   begin
      Result.Last := Right.Last + 1;

      Result.Reference := new String (1 .. Result.Last);
      Result.Reference (1) := Left;
      Result.Reference (2 .. Result.Last) :=
        Right.Reference (1 .. Right.Last);
      return Result;
   end "&";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character) return Unbounded_String
   is
      Result : Unbounded_String;

   begin
      Result.Last   := Left;

      Result.Reference := new String (1 .. Left);
      for J in Result.Reference'Range loop
         Result.Reference (J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : String) return Unbounded_String
   is
      Len    : constant Natural := Right'Length;
      K      : Positive;
      Result : Unbounded_String;

   begin
      Result.Last := Left * Len;

      Result.Reference := new String (1 .. Result.Last);

      K := 1;
      for J in 1 .. Left loop
         Result.Reference (K .. K + Len - 1) := Right;
         K := K + Len;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Unbounded_String) return Unbounded_String
   is
      Len    : constant Natural := Right.Last;
      K      : Positive;
      Result : Unbounded_String;

   begin
      Result.Last := Left * Len;

      Result.Reference := new String (1 .. Result.Last);

      K := 1;
      for J in 1 .. Left loop
         Result.Reference (K .. K + Len - 1) :=
           Right.Reference (1 .. Right.Last);
         K := K + Len;
      end loop;

      return Result;
   end "*";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) < Right.Reference (1 .. Right.Last);
   end "<";

   function "<"
     (Left  : Unbounded_String;
      Right : String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) < Right;
   end "<";

   function "<"
     (Left  : String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return Left < Right.Reference (1 .. Right.Last);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) <= Right.Reference (1 .. Right.Last);
   end "<=";

   function "<="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) <= Right;
   end "<=";

   function "<="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return Left <= Right.Reference (1 .. Right.Last);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) = Right.Reference (1 .. Right.Last);
   end "=";

   function "="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) = Right;
   end "=";

   function "="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return Left = Right.Reference (1 .. Right.Last);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) > Right.Reference (1 .. Right.Last);
   end ">";

   function ">"
     (Left  : Unbounded_String;
      Right : String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) > Right;
   end ">";

   function ">"
     (Left  : String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return Left > Right.Reference (1 .. Right.Last);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) >= Right.Reference (1 .. Right.Last);
   end ">=";

   function ">="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) >= Right;
   end ">=";

   function ">="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   is
   begin
      return Left >= Right.Reference (1 .. Right.Last);
   end ">=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Unbounded_String) is
   begin
      --  Copy string, except we do not copy the statically allocated null
      --  string since it can never be deallocated. Note that we do not copy
      --  extra string room here to avoid dragging unused allocated memory.

      if Object.Reference /= Null_String'Access then
         Object.Reference := new String'(Object.Reference (1 .. Object.Last));
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Unbounded_String)
   is
   begin
      Realloc_For_Chunk (Source, New_Item.Last);
      Source.Reference (Source.Last + 1 .. Source.Last + New_Item.Last) :=
        New_Item.Reference (1 .. New_Item.Last);
      Source.Last := Source.Last + New_Item.Last;
   end Append;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : String)
   is
   begin
      Realloc_For_Chunk (Source, New_Item'Length);
      Source.Reference (Source.Last + 1 .. Source.Last + New_Item'Length) :=
        New_Item;
      Source.Last := Source.Last + New_Item'Length;
   end Append;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Character)
   is
   begin
      Realloc_For_Chunk (Source, 1);
      Source.Reference (Source.Last + 1) := New_Item;
      Source.Last := Source.Last + 1;
   end Append;

   -----------
   -- Count --
   -----------

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      return
        Search.Count (Source.Reference (1 .. Source.Last), Pattern, Mapping);
   end Count;

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return
        Search.Count (Source.Reference (1 .. Source.Last), Pattern, Mapping);
   end Count;

   function Count
     (Source : Unbounded_String;
      Set    : Maps.Character_Set) return Natural
   is
   begin
      return Search.Count (Source.Reference (1 .. Source.Last), Set);
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural) return Unbounded_String
   is
   begin
      return
        To_Unbounded_String
          (Fixed.Delete (Source.Reference (1 .. Source.Last), From, Through));
   end Delete;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : Positive;
      Through : Natural)
   is
   begin
      if From > Through then
         null;

      elsif From < Source.Reference'First or else Through > Source.Last then
         raise Index_Error;

      else
         declare
            Len : constant Natural := Through - From + 1;

         begin
            Source.Reference (From .. Source.Last - Len) :=
              Source.Reference (Through + 1 .. Source.Last);
            Source.Last := Source.Last - Len;
         end;
      end if;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Unbounded_String;
      Index  : Positive) return Character
   is
   begin
      if Index <= Source.Last then
         return Source.Reference (Index);
      else
         raise Strings.Index_Error;
      end if;
   end Element;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Unbounded_String) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (String, String_Access);

   begin
      --  Note: Don't try to free statically allocated null string

      if Object.Reference /= Null_String'Access then
         Deallocate (Object.Reference);
         Object.Reference := Null_Unbounded_String.Reference;
         Object.Last := 0;
      end if;
   end Finalize;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Search.Find_Token
        (Source.Reference (From .. Source.Last), Set, Test, First, Last);
   end Find_Token;

   procedure Find_Token
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Search.Find_Token
        (Source.Reference (1 .. Source.Last), Set, Test, First, Last);
   end Find_Token;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out String_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (String, String_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Unbounded_String.Reference then
         Deallocate (X);
      end if;
   end Free;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Head (Source.Reference (1 .. Source.Last), Count, Pad));
   end Head;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   is
      Old : String_Access := Source.Reference;
   begin
      Source.Reference :=
        new String'(Fixed.Head (Source.Reference (1 .. Source.Last),
                    Count, Pad));
      Source.Last := Source.Reference'Length;
      Free (Old);
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Strings.Direction := Strings.Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      return Search.Index
        (Source.Reference (1 .. Source.Last), Pattern, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return Search.Index
        (Source.Reference (1 .. Source.Last), Pattern, Going, Mapping);
   end Index;

   function Index
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward) return Natural
   is
   begin
      return Search.Index
        (Source.Reference (1 .. Source.Last), Set, Test, Going);
   end Index;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      return Search.Index
        (Source.Reference (1 .. Source.Last), Pattern, From, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return Search.Index
        (Source.Reference (1 .. Source.Last), Pattern, From, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   is
   begin
      return Search.Index
        (Source.Reference (1 .. Source.Last), Set, From, Test, Going);
   end Index;

   function Index_Non_Blank
     (Source : Unbounded_String;
      Going  : Strings.Direction := Strings.Forward) return Natural
   is
   begin
      return
        Search.Index_Non_Blank
          (Source.Reference (1 .. Source.Last), Going);
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : Unbounded_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
      return
        Search.Index_Non_Blank
          (Source.Reference (1 .. Source.Last), From, Going);
   end Index_Non_Blank;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Unbounded_String) is
   begin
      Object.Reference := Null_Unbounded_String.Reference;
      Object.Last      := 0;
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Insert (Source.Reference (1 .. Source.Last), Before, New_Item));
   end Insert;

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : Positive;
      New_Item : String)
   is
   begin
      if Before not in Source.Reference'First .. Source.Last + 1 then
         raise Index_Error;
      end if;

      Realloc_For_Chunk (Source, New_Item'Length);

      Source.Reference
        (Before +  New_Item'Length .. Source.Last + New_Item'Length) :=
           Source.Reference (Before .. Source.Last);

      Source.Reference (Before .. Before + New_Item'Length - 1) := New_Item;
      Source.Last := Source.Last + New_Item'Length;
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_String) return Natural is
   begin
      return Source.Last;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Unbounded_String;
      Position : Positive;
      New_Item : String) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Overwrite
          (Source.Reference (1 .. Source.Last), Position, New_Item));
   end Overwrite;

   procedure Overwrite
     (Source    : in out Unbounded_String;
      Position  : Positive;
      New_Item  : String)
   is
      NL : constant Natural := New_Item'Length;
   begin
      if Position <= Source.Last - NL + 1 then
         Source.Reference (Position .. Position + NL - 1) := New_Item;
      else
         declare
            Old : String_Access := Source.Reference;
         begin
            Source.Reference := new String'
              (Fixed.Overwrite
                (Source.Reference (1 .. Source.Last), Position, New_Item));
            Source.Last := Source.Reference'Length;
            Free (Old);
         end;
      end if;
   end Overwrite;

   -----------------------
   -- Realloc_For_Chunk --
   -----------------------

   procedure Realloc_For_Chunk
     (Source     : in out Unbounded_String;
      Chunk_Size : Natural)
   is
      Growth_Factor : constant := 32;
      --  The growth factor controls how much extra space is allocated when
      --  we have to increase the size of an allocated unbounded string. By
      --  allocating extra space, we avoid the need to reallocate on every
      --  append, particularly important when a string is built up by repeated
      --  append operations of small pieces. This is expressed as a factor so
      --  32 means add 1/32 of the length of the string as growth space.

      Min_Mul_Alloc : constant := Standard'Maximum_Alignment;
      --  Allocation will be done by a multiple of Min_Mul_Alloc This causes
      --  no memory loss as most (all?) malloc implementations are obliged to
      --  align the returned memory on the maximum alignment as malloc does not
      --  know the target alignment.

      S_Length : constant Natural := Source.Reference'Length;

   begin
      if Chunk_Size > S_Length - Source.Last then
         declare
            New_Size : constant Positive :=
              S_Length + Chunk_Size + (S_Length / Growth_Factor);

            New_Rounded_Up_Size : constant Positive :=
              ((New_Size - 1) / Min_Mul_Alloc + 1) * Min_Mul_Alloc;

            Tmp : constant String_Access :=
              new String (1 .. New_Rounded_Up_Size);

         begin
            Tmp (1 .. Source.Last) := Source.Reference (1 .. Source.Last);
            Free (Source.Reference);
            Source.Reference := Tmp;
         end;
      end if;
   end Realloc_For_Chunk;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : Positive;
      By     : Character)
   is
   begin
      if Index <= Source.Last then
         Source.Reference (Index) := By;
      else
         raise Strings.Index_Error;
      end if;
   end Replace_Element;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Replace_Slice
           (Source.Reference (1 .. Source.Last), Low, High, By));
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in out Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String)
   is
      Old : String_Access := Source.Reference;
   begin
      Source.Reference := new String'
        (Fixed.Replace_Slice
           (Source.Reference (1 .. Source.Last), Low, High, By));
      Source.Last := Source.Reference'Length;
      Free (Old);
   end Replace_Slice;

   --------------------------
   -- Set_Unbounded_String --
   --------------------------

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String)
   is
      Old : String_Access := Target.Reference;
   begin
      Target.Last          := Source'Length;
      Target.Reference     := new String (1 .. Source'Length);
      Target.Reference.all := Source;
      Free (Old);
   end Set_Unbounded_String;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String
   is
   begin
      --  Note: test of High > Length is in accordance with AI95-00128

      if Low > Source.Last + 1 or else High > Source.Last then
         raise Index_Error;
      else
         return Source.Reference (Low .. High);
      end if;
   end Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String is
   begin
      return To_Unbounded_String
        (Fixed.Tail (Source.Reference (1 .. Source.Last), Count, Pad));
   end Tail;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   is
      Old : String_Access := Source.Reference;
   begin
      Source.Reference := new String'
        (Fixed.Tail (Source.Reference (1 .. Source.Last), Count, Pad));
      Source.Last := Source.Reference'Length;
      Free (Old);
   end Tail;

   ---------------
   -- To_String --
   ---------------

   function To_String (Source : Unbounded_String) return String is
   begin
      return Source.Reference (1 .. Source.Last);
   end To_String;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String (Source : String) return Unbounded_String is
      Result : Unbounded_String;
   begin
      --  Do not allocate an empty string: keep the default

      if Source'Length > 0 then
         Result.Last          := Source'Length;
         Result.Reference     := new String (1 .. Source'Length);
         Result.Reference.all := Source;
      end if;

      return Result;
   end To_Unbounded_String;

   function To_Unbounded_String
     (Length : Natural) return Unbounded_String
   is
      Result : Unbounded_String;

   begin
      --  Do not allocate an empty string: keep the default

      if Length > 0 then
         Result.Last      := Length;
         Result.Reference := new String (1 .. Length);
      end if;

      return Result;
   end To_Unbounded_String;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Translate (Source.Reference (1 .. Source.Last), Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping)
   is
   begin
      Fixed.Translate (Source.Reference (1 .. Source.Last), Mapping);
   end Translate;

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping_Function) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Translate (Source.Reference (1 .. Source.Last), Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping_Function)
   is
   begin
      Fixed.Translate (Source.Reference (1 .. Source.Last), Mapping);
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Unbounded_String;
      Side   : Trim_End) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Trim (Source.Reference (1 .. Source.Last), Side));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_String;
      Side   : Trim_End)
   is
      Old : String_Access := Source.Reference;
   begin
      Source.Reference := new String'
        (Fixed.Trim (Source.Reference (1 .. Source.Last), Side));
      Source.Last      := Source.Reference'Length;
      Free (Old);
   end Trim;

   function Trim
     (Source : Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Unbounded_String
   is
   begin
      return To_Unbounded_String
        (Fixed.Trim (Source.Reference (1 .. Source.Last), Left, Right));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set)
   is
      Old : String_Access := Source.Reference;
   begin
      Source.Reference := new String'
        (Fixed.Trim (Source.Reference (1 .. Source.Last), Left, Right));
      Source.Last      := Source.Reference'Length;
      Free (Old);
   end Trim;

   ---------------------
   -- Unbounded_Slice --
   ---------------------

   function Unbounded_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return Unbounded_String
   is
   begin
      if Low > Source.Last + 1 or else High > Source.Last then
         raise Index_Error;
      else
         return To_Unbounded_String (Source.Reference.all (Low .. High));
      end if;
   end Unbounded_Slice;

   procedure Unbounded_Slice
     (Source : Unbounded_String;
      Target : out Unbounded_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      if Low > Source.Last + 1 or else High > Source.Last then
         raise Index_Error;
      else
         Target := To_Unbounded_String (Source.Reference.all (Low .. High));
      end if;
   end Unbounded_Slice;

end Ada.Strings.Unbounded;
