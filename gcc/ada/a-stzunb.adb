------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . S T R I N G S . W I D E _ W I D E _ U N B O U N D E D       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Search;
with Ada.Unchecked_Deallocation;

package body Ada.Strings.Wide_Wide_Unbounded is

   use Ada.Finalization;

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      L_Length : constant Natural := Left.Last;
      R_Length : constant Natural := Right.Last;
      Result   : Unbounded_Wide_Wide_String;

   begin
      Result.Last := L_Length + R_Length;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);

      Result.Reference (1 .. L_Length) :=
        Left.Reference (1 .. Left.Last);
      Result.Reference (L_Length + 1 .. Result.Last) :=
        Right.Reference (1 .. Right.Last);

      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      L_Length : constant Natural := Left.Last;
      Result   : Unbounded_Wide_Wide_String;

   begin
      Result.Last := L_Length + Right'Length;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);

      Result.Reference (1 .. L_Length) := Left.Reference (1 .. Left.Last);
      Result.Reference (L_Length + 1 .. Result.Last) := Right;

      return Result;
   end "&";

   function "&"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      R_Length : constant Natural := Right.Last;
      Result   : Unbounded_Wide_Wide_String;

   begin
      Result.Last := Left'Length + R_Length;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);

      Result.Reference (1 .. Left'Length) := Left;
      Result.Reference (Left'Length + 1 .. Result.Last) :=
        Right.Reference (1 .. Right.Last);

      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;

   begin
      Result.Last := Left.Last + 1;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);

      Result.Reference (1 .. Result.Last - 1) :=
        Left.Reference (1 .. Left.Last);
      Result.Reference (Result.Last) := Right;

      return Result;
   end "&";

   function "&"
     (Left  : Wide_Wide_Character;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;

   begin
      Result.Last := Right.Last + 1;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);
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
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;

   begin
      Result.Last   := Left;

      Result.Reference := new Wide_Wide_String (1 .. Left);
      for J in Result.Reference'Range loop
         Result.Reference (J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Len    : constant Natural := Right'Length;
      K      : Positive;
      Result : Unbounded_Wide_Wide_String;

   begin
      Result.Last := Left * Len;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);

      K := 1;
      for J in 1 .. Left loop
         Result.Reference (K .. K + Len - 1) := Right;
         K := K + Len;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Len    : constant Natural := Right.Last;
      K      : Positive;
      Result : Unbounded_Wide_Wide_String;

   begin
      Result.Last := Left * Len;

      Result.Reference := new Wide_Wide_String (1 .. Result.Last);

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
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) < Right.Reference (1 .. Right.Last);
   end "<";

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) < Right;
   end "<";

   function "<"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left < Right.Reference (1 .. Right.Last);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) <= Right.Reference (1 .. Right.Last);
   end "<=";

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) <= Right;
   end "<=";

   function "<="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left <= Right.Reference (1 .. Right.Last);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) = Right.Reference (1 .. Right.Last);
   end "=";

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) = Right;
   end "=";

   function "="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left = Right.Reference (1 .. Right.Last);
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) > Right.Reference (1 .. Right.Last);
   end ">";

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) > Right;
   end ">";

   function ">"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left > Right.Reference (1 .. Right.Last);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return
        Left.Reference (1 .. Left.Last) >= Right.Reference (1 .. Right.Last);
   end ">=";

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean
   is
   begin
      return Left.Reference (1 .. Left.Last) >= Right;
   end ">=";

   function ">="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean
   is
   begin
      return Left >= Right.Reference (1 .. Right.Last);
   end ">=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Unbounded_Wide_Wide_String) is
   begin
      --  Copy string, except we do not copy the statically allocated null
      --  string, since it can never be deallocated. Note that we do not copy
      --  extra string room here to avoid dragging unused allocated memory.

      if Object.Reference /= Null_Wide_Wide_String'Access then
         Object.Reference :=
           new Wide_Wide_String'(Object.Reference (1 .. Object.Last));
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item : Unbounded_Wide_Wide_String)
   is
   begin
      Realloc_For_Chunk (Source, New_Item.Last);
      Source.Reference (Source.Last + 1 .. Source.Last + New_Item.Last) :=
        New_Item.Reference (1 .. New_Item.Last);
      Source.Last := Source.Last + New_Item.Last;
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_String)
   is
   begin
      Realloc_For_Chunk (Source, New_Item'Length);
      Source.Reference (Source.Last + 1 .. Source.Last + New_Item'Length) :=
        New_Item;
      Source.Last := Source.Last + New_Item'Length;
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_Character)
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
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural
   is
   begin
      return
        Wide_Wide_Search.Count
          (Source.Reference (1 .. Source.Last), Pattern, Mapping);
   end Count;

   function Count
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural
   is
   begin
      return
        Wide_Wide_Search.Count
          (Source.Reference (1 .. Source.Last), Pattern, Mapping);
   end Count;

   function Count
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural
   is
   begin
      return
        Wide_Wide_Search.Count
        (Source.Reference (1 .. Source.Last), Set);
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Delete
             (Source.Reference (1 .. Source.Last), From, Through));
   end Delete;

   procedure Delete
     (Source  : in out Unbounded_Wide_Wide_String;
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
     (Source : Unbounded_Wide_Wide_String;
      Index  : Positive) return Wide_Wide_Character
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

   procedure Finalize (Object : in out Unbounded_Wide_Wide_String) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Wide_Wide_String, Wide_Wide_String_Access);

   begin
      --  Note: Don't try to free statically allocated null string

      if Object.Reference /= Null_Wide_Wide_String'Access then
         Deallocate (Object.Reference);
         Object.Reference := Null_Unbounded_Wide_Wide_String.Reference;
         Object.Last := 0;
      end if;
   end Finalize;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Wide_Wide_Search.Find_Token
        (Source.Reference (1 .. Source.Last), Set, Test, First, Last);
   end Find_Token;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Wide_Wide_String_Access) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation
          (Wide_Wide_String, Wide_Wide_String_Access);

   begin
      --  Note: Do not try to free statically allocated null string

      if X /= Null_Unbounded_Wide_Wide_String.Reference then
         Deallocate (X);
      end if;
   end Free;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space)
      return Unbounded_Wide_Wide_String
   is
   begin
      return To_Unbounded_Wide_Wide_String
        (Wide_Wide_Fixed.Head
           (Source.Reference (1 .. Source.Last), Count, Pad));
   end Head;

   procedure Head
     (Source : in out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space)
   is
      Old : Wide_Wide_String_Access := Source.Reference;
   begin
      Source.Reference :=
        new Wide_Wide_String'
          (Wide_Wide_Fixed.Head
             (Source.Reference (1 .. Source.Last), Count, Pad));
      Source.Last := Source.Reference'Length;
      Free (Old);
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Strings.Direction := Strings.Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural
   is
   begin
      return
        Wide_Wide_Search.Index
          (Source.Reference (1 .. Source.Last), Pattern, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural
   is
   begin
      return
        Wide_Wide_Search.Index
          (Source.Reference (1 .. Source.Last), Pattern, Going, Mapping);
   end Index;

   function Index
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward) return Natural
   is
   begin
      return Wide_Wide_Search.Index
        (Source.Reference (1 .. Source.Last), Set, Test, Going);
   end Index;

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural
   is
   begin
      return
        Wide_Wide_Search.Index
          (Source.Reference (1 .. Source.Last), Pattern, From, Going, Mapping);
   end Index;

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural
   is
   begin
      return
        Wide_Wide_Search.Index
          (Source.Reference (1 .. Source.Last), Pattern, From, Going, Mapping);
   end Index;

   function Index
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From   : Positive;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      return
        Wide_Wide_Search.Index
          (Source.Reference (1 .. Source.Last), Set, From, Test, Going);
   end Index;

   function Index_Non_Blank
     (Source : Unbounded_Wide_Wide_String;
      Going  : Strings.Direction := Strings.Forward) return Natural
   is
   begin
      return
        Wide_Wide_Search.Index_Non_Blank
          (Source.Reference (1 .. Source.Last), Going);
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : Unbounded_Wide_Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
      return
        Wide_Wide_Search.Index_Non_Blank
          (Source.Reference (1 .. Source.Last), From, Going);
   end Index_Non_Blank;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Unbounded_Wide_Wide_String) is
   begin
      Object.Reference := Null_Unbounded_Wide_Wide_String.Reference;
      Object.Last      := 0;
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Insert
             (Source.Reference (1 .. Source.Last), Before, New_Item));
   end Insert;

   procedure Insert
     (Source   : in out Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String)
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

   function Length (Source : Unbounded_Wide_Wide_String) return Natural is
   begin
      return Source.Last;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Overwrite
            (Source.Reference (1 .. Source.Last), Position, New_Item));
   end Overwrite;

   procedure Overwrite
     (Source    : in out Unbounded_Wide_Wide_String;
      Position  : Positive;
      New_Item  : Wide_Wide_String)
   is
      NL : constant Natural := New_Item'Length;
   begin
      if Position <= Source.Last - NL + 1 then
         Source.Reference (Position .. Position + NL - 1) := New_Item;
      else
         declare
            Old : Wide_Wide_String_Access := Source.Reference;
         begin
            Source.Reference := new Wide_Wide_String'
              (Wide_Wide_Fixed.Overwrite
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
     (Source     : in out Unbounded_Wide_Wide_String;
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
                                    ((New_Size - 1) / Min_Mul_Alloc + 1) *
                                       Min_Mul_Alloc;

            Tmp : constant Wide_Wide_String_Access :=
                    new Wide_Wide_String (1 .. New_Rounded_Up_Size);

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
     (Source : in out Unbounded_Wide_Wide_String;
      Index  : Positive;
      By     : Wide_Wide_Character)
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
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
   begin
      return To_Unbounded_Wide_Wide_String
        (Wide_Wide_Fixed.Replace_Slice
           (Source.Reference (1 .. Source.Last), Low, High, By));
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String)
   is
      Old : Wide_Wide_String_Access := Source.Reference;
   begin
      Source.Reference := new Wide_Wide_String'
        (Wide_Wide_Fixed.Replace_Slice
           (Source.Reference (1 .. Source.Last), Low, High, By));
      Source.Last := Source.Reference'Length;
      Free (Old);
   end Replace_Slice;

   ------------------------------------
   -- Set_Unbounded_Wide_Wide_String --
   ------------------------------------

   procedure Set_Unbounded_Wide_Wide_String
     (Target : out Unbounded_Wide_Wide_String;
      Source : Wide_Wide_String)
   is
   begin
      Target.Last          := Source'Length;
      Target.Reference     := new Wide_Wide_String (1 .. Source'Length);
      Target.Reference.all := Source;
   end Set_Unbounded_Wide_Wide_String;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_Wide_String
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
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space)
      return Unbounded_Wide_Wide_String is
   begin
      return To_Unbounded_Wide_Wide_String
        (Wide_Wide_Fixed.Tail
           (Source.Reference (1 .. Source.Last), Count, Pad));
   end Tail;

   procedure Tail
     (Source : in out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space)
   is
      Old : Wide_Wide_String_Access := Source.Reference;
   begin
      Source.Reference := new Wide_Wide_String'
        (Wide_Wide_Fixed.Tail
           (Source.Reference (1 .. Source.Last), Count, Pad));
      Source.Last := Source.Reference'Length;
      Free (Old);
   end Tail;

   -----------------------------------
   -- To_Unbounded_Wide_Wide_String --
   -----------------------------------

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
   begin
      Result.Last          := Source'Length;
      Result.Reference     := new Wide_Wide_String (1 .. Source'Length);
      Result.Reference.all := Source;
      return Result;
   end To_Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Length : Natural) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String;
   begin
      Result.Last      := Length;
      Result.Reference := new Wide_Wide_String (1 .. Length);
      return Result;
   end To_Unbounded_Wide_Wide_String;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String) return Wide_Wide_String
   is
   begin
      return Source.Reference (1 .. Source.Last);
   end To_Wide_Wide_String;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Translate
             (Source.Reference (1 .. Source.Last), Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
   is
   begin
      Wide_Wide_Fixed.Translate (Source.Reference (1 .. Source.Last), Mapping);
   end Translate;

   function Translate
     (Source  : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Translate
            (Source.Reference (1 .. Source.Last), Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
   is
   begin
      Wide_Wide_Fixed.Translate (Source.Reference (1 .. Source.Last), Mapping);
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Trim (Source.Reference (1 .. Source.Last), Side));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_Wide_Wide_String;
      Side   : Trim_End)
   is
      Old : Wide_Wide_String_Access := Source.Reference;
   begin
      Source.Reference :=
        new Wide_Wide_String'
          (Wide_Wide_Fixed.Trim (Source.Reference (1 .. Source.Last), Side));
      Source.Last      := Source.Reference'Length;
      Free (Old);
   end Trim;

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Unbounded_Wide_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_Wide_String
          (Wide_Wide_Fixed.Trim
             (Source.Reference (1 .. Source.Last), Left, Right));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
   is
      Old : Wide_Wide_String_Access := Source.Reference;
   begin
      Source.Reference :=
        new Wide_Wide_String'
          (Wide_Wide_Fixed.Trim
             (Source.Reference (1 .. Source.Last), Left, Right));
      Source.Last      := Source.Reference'Length;
      Free (Old);
   end Trim;

   ---------------------
   -- Unbounded_Slice --
   ---------------------

   function Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_Wide_String
   is
   begin
      if Low > Source.Last + 1 or else High > Source.Last then
         raise Index_Error;
      else
         return
           To_Unbounded_Wide_Wide_String (Source.Reference.all (Low .. High));
      end if;
   end Unbounded_Slice;

   procedure Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Target : out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      if Low > Source.Last + 1 or else High > Source.Last then
         raise Index_Error;
      else
         Target :=
           To_Unbounded_Wide_Wide_String (Source.Reference.all (Low .. High));
      end if;
   end Unbounded_Slice;

end Ada.Strings.Wide_Wide_Unbounded;
