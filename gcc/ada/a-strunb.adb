------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                A D A . S T R I N G S . U N B O U N D E D                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
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

with Ada.Strings.Fixed;
with Ada.Strings.Search;
with Ada.Unchecked_Deallocation;

package body Ada.Strings.Unbounded is

   use Ada.Finalization;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Unbounded_String) return Unbounded_String is
      L_Length : constant Integer := Left.Reference.all'Length;
      R_Length : constant Integer := Right.Reference.all'Length;
      Length   : constant Integer :=  L_Length + R_Length;
      Result   : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Length);
      Result.Reference.all (1 .. L_Length)          := Left.Reference.all;
      Result.Reference.all (L_Length + 1 .. Length) := Right.Reference.all;
      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_String;
      Right : String)
      return  Unbounded_String
   is
      L_Length : constant Integer := Left.Reference.all'Length;
      Length   : constant Integer := L_Length +  Right'Length;
      Result   : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Length);
      Result.Reference.all (1 .. L_Length)          := Left.Reference.all;
      Result.Reference.all (L_Length + 1 .. Length) := Right;
      return Result;
   end "&";

   function "&"
     (Left  : String;
      Right : Unbounded_String)
      return  Unbounded_String
   is
      R_Length : constant Integer := Right.Reference.all'Length;
      Length   : constant Integer := Left'Length + R_Length;
      Result   : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Length);
      Result.Reference.all (1 .. Left'Length)          := Left;
      Result.Reference.all (Left'Length + 1 .. Length) := Right.Reference.all;
      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_String;
      Right : Character)
      return  Unbounded_String
   is
      Length : constant Integer := Left.Reference.all'Length + 1;
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Length);
      Result.Reference.all (1 .. Length - 1) := Left.Reference.all;
      Result.Reference.all (Length)          := Right;
      return Result;
   end "&";

   function "&"
     (Left  : Character;
      Right : Unbounded_String)
      return  Unbounded_String
   is
      Length : constant Integer := Right.Reference.all'Length + 1;
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Length);
      Result.Reference.all (1)           := Left;
      Result.Reference.all (2 .. Length) := Right.Reference.all;
      return Result;
   end "&";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character)
      return  Unbounded_String
   is
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Left);
      for J in Result.Reference'Range loop
         Result.Reference (J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : String)
     return   Unbounded_String
   is
      Len    : constant Integer := Right'Length;
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Left * Len);
      for J in 1 .. Left loop
         Result.Reference.all (Len * J - Len + 1 .. Len * J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Unbounded_String)
      return  Unbounded_String
   is
      Len    : constant Integer := Right.Reference.all'Length;
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Left * Len);
      for I in 1 .. Left loop
         Result.Reference.all (Len * I - Len + 1 .. Len * I) :=
           Right.Reference.all;
      end loop;

      return Result;
   end "*";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : in Unbounded_String) return Boolean is
   begin
      return Left.Reference.all < Right.Reference.all;
   end "<";

   function "<"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean
   is
   begin
      return Left.Reference.all < Right;
   end "<";

   function "<"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean
   is
   begin
      return Left < Right.Reference.all;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : in Unbounded_String) return Boolean is
   begin
      return Left.Reference.all <= Right.Reference.all;
   end "<=";

   function "<="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean
   is
   begin
      return Left.Reference.all <= Right;
   end "<=";

   function "<="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean
   is
   begin
      return Left <= Right.Reference.all;
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Unbounded_String) return Boolean is
   begin
      return Left.Reference.all = Right.Reference.all;
   end "=";

   function "="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean
   is
   begin
      return Left.Reference.all = Right;
   end "=";

   function "="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean
   is
   begin
      return Left = Right.Reference.all;
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"  (Left, Right : in Unbounded_String) return Boolean is
   begin
      return Left.Reference.all > Right.Reference.all;
   end ">";

   function ">"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean
   is
   begin
      return Left.Reference.all > Right;
   end ">";

   function ">"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean
   is
   begin
      return Left > Right.Reference.all;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : in Unbounded_String) return Boolean is
   begin
      return Left.Reference.all >= Right.Reference.all;
   end ">=";

   function ">="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean
   is
   begin
      return Left.Reference.all >= Right;
   end ">=";

   function ">="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean
   is
   begin
      return Left >= Right.Reference.all;
   end ">=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Unbounded_String) is
   begin
      --  Copy string, except we do not copy the statically allocated null
      --  string, since it can never be deallocated.

      if Object.Reference /= Null_String'Access then
         Object.Reference := new String'(Object.Reference.all);
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in Unbounded_String)
   is
      S_Length : constant Integer := Source.Reference.all'Length;
      Length   : constant Integer := S_Length + New_Item.Reference.all'Length;
      Tmp      : String_Access;

   begin
      Tmp := new String (1 .. Length);
      Tmp (1 .. S_Length) := Source.Reference.all;
      Tmp (S_Length + 1 .. Length) := New_Item.Reference.all;
      Free (Source.Reference);
      Source.Reference := Tmp;
   end Append;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in String)
   is
      S_Length : constant Integer := Source.Reference.all'Length;
      Length   : constant Integer := S_Length + New_Item'Length;
      Tmp      : String_Access;

   begin
      Tmp := new String (1 .. Length);
      Tmp (1 .. S_Length) := Source.Reference.all;
      Tmp (S_Length + 1 .. Length) := New_Item;
      Free (Source.Reference);
      Source.Reference := Tmp;
   end Append;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in Character)
   is
      S_Length : constant Integer := Source.Reference.all'Length;
      Length   : constant Integer := S_Length + 1;
      Tmp      : String_Access;

   begin
      Tmp := new String (1 .. Length);
      Tmp (1 .. S_Length) := Source.Reference.all;
      Tmp (S_Length + 1) := New_Item;
      Free (Source.Reference);
      Source.Reference := Tmp;
   end Append;

   -----------
   -- Count --
   -----------

   function Count
     (Source   : Unbounded_String;
      Pattern  : String;
      Mapping  : Maps.Character_Mapping := Maps.Identity)
      return     Natural
   is
   begin
      return Search.Count (Source.Reference.all, Pattern, Mapping);
   end Count;

   function Count
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural
   is
   begin
      return Search.Count (Source.Reference.all, Pattern, Mapping);
   end Count;

   function Count
     (Source   : Unbounded_String;
      Set      : Maps.Character_Set)
      return     Natural
   is
   begin
      return Search.Count (Source.Reference.all, Set);
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural)
      return    Unbounded_String
   is
   begin
      return
        To_Unbounded_String
          (Fixed.Delete (Source.Reference.all, From, Through));
   end Delete;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : in Positive;
      Through : in Natural)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference :=
        new String' (Fixed.Delete (Old.all, From, Through));
      Free (Old);
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Unbounded_String;
      Index  : Positive)
      return   Character
   is
   begin
      if Index <= Source.Reference.all'Last then
         return Source.Reference.all (Index);
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
      end if;
   end Finalize;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Search.Find_Token (Source.Reference.all, Set, Test, First, Last);
   end Find_Token;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out String_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (String, String_Access);

   begin
      --  Note: Don't try to free statically allocated null string

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
      Pad    : Character := Space)
      return   Unbounded_String
   is
   begin
      return
        To_Unbounded_String (Fixed.Head (Source.Reference.all, Count, Pad));
   end Head;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference := new String'(Fixed.Head (Old.all, Count, Pad));
      Free (Old);
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source   : Unbounded_String;
      Pattern  : String;
      Going    : Strings.Direction := Strings.Forward;
      Mapping  : Maps.Character_Mapping := Maps.Identity)
      return     Natural
   is
   begin
      return Search.Index (Source.Reference.all, Pattern, Going, Mapping);
   end Index;

   function Index
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping_Function)
      return Natural
   is
   begin
      return Search.Index (Source.Reference.all, Pattern, Going, Mapping);
   end Index;

   function Index
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward)
      return   Natural
   is
   begin
      return Search.Index (Source.Reference.all, Set, Test, Going);
   end Index;

   function Index_Non_Blank
     (Source : Unbounded_String;
      Going  : Strings.Direction := Strings.Forward)
      return   Natural
   is
   begin
      return Search.Index_Non_Blank (Source.Reference.all, Going);
   end Index_Non_Blank;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Unbounded_String) is
   begin
      Object.Reference := Null_Unbounded_String.Reference;
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String)
      return     Unbounded_String
   is
   begin
      return
        To_Unbounded_String
          (Fixed.Insert (Source.Reference.all, Before, New_Item));
   end Insert;

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : in Positive;
      New_Item : in String)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference :=
        new String' (Fixed.Insert (Source.Reference.all, Before, New_Item));
      Free (Old);
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_String) return Natural is
   begin
      return Source.Reference.all'Length;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source    : Unbounded_String;
      Position  : Positive;
      New_Item  : String)
      return      Unbounded_String is

   begin
      return To_Unbounded_String
        (Fixed.Overwrite (Source.Reference.all, Position, New_Item));
   end Overwrite;

   procedure Overwrite
     (Source    : in out Unbounded_String;
      Position  : in Positive;
      New_Item  : in String)
   is
      NL : constant Integer := New_Item'Length;

   begin
      if Position <= Source.Reference'Length - NL + 1 then
         Source.Reference (Position .. Position + NL - 1) := New_Item;

      else
         declare
            Old : String_Access := Source.Reference;

         begin
            Source.Reference := new
              String'(Fixed.Overwrite (Old.all, Position, New_Item));
            Free (Old);
         end;
      end if;
   end Overwrite;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : Positive;
      By     : Character)
   is
   begin
      if Index <= Source.Reference.all'Last then
         Source.Reference.all (Index) := By;
      else
         raise Strings.Index_Error;
      end if;
   end Replace_Element;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source   : Unbounded_String;
      Low      : Positive;
      High     : Natural;
      By       : String)
      return     Unbounded_String
   is
   begin
      return
        To_Unbounded_String
          (Fixed.Replace_Slice (Source.Reference.all, Low, High, By));
   end Replace_Slice;

   procedure Replace_Slice
     (Source   : in out Unbounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in String)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference :=
        new String'(Fixed.Replace_Slice (Old.all, Low, High, By));
      Free (Old);
   end Replace_Slice;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural)
      return   String
   is
      Length : constant Natural := Source.Reference'Length;

   begin
      --  Note: test of High > Length is in accordance with AI95-00128

      if Low > Length + 1 or else High > Length then
         raise Index_Error;
      else
         return Source.Reference.all (Low .. High);
      end if;
   end Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
      return   Unbounded_String is

   begin
      return
        To_Unbounded_String (Fixed.Tail (Source.Reference.all, Count, Pad));
   end Tail;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference := new String'(Fixed.Tail (Old.all, Count, Pad));
      Free (Old);
   end Tail;

   ---------------
   -- To_String --
   ---------------

   function To_String (Source : Unbounded_String) return String is
   begin
      return Source.Reference.all;
   end To_String;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String (Source : String) return Unbounded_String is
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Source'Length);
      Result.Reference.all := Source;
      return Result;
   end To_Unbounded_String;

   function To_Unbounded_String
     (Length : in Natural)
      return   Unbounded_String
   is
      Result : Unbounded_String;

   begin
      Result.Reference := new String (1 .. Length);
      return Result;
   end To_Unbounded_String;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping)
      return    Unbounded_String
   is
   begin
      return
        To_Unbounded_String (Fixed.Translate (Source.Reference.all, Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping)
   is
   begin
      Fixed.Translate (Source.Reference.all, Mapping);
   end Translate;

   function Translate
     (Source  : in Unbounded_String;
      Mapping : in Maps.Character_Mapping_Function)
      return    Unbounded_String
   is
   begin
      return
        To_Unbounded_String (Fixed.Translate (Source.Reference.all, Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : in Maps.Character_Mapping_Function)
   is
   begin
      Fixed.Translate (Source.Reference.all, Mapping);
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : in Unbounded_String;
      Side   : in Trim_End)
      return   Unbounded_String
   is
   begin
      return To_Unbounded_String (Fixed.Trim (Source.Reference.all, Side));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_String;
      Side   : in Trim_End)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference := new String'(Fixed.Trim (Old.all, Side));
      Free (Old);
   end Trim;

   function Trim
     (Source : in Unbounded_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set)
      return   Unbounded_String
   is
   begin
      return
        To_Unbounded_String (Fixed.Trim (Source.Reference.all, Left, Right));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set)
   is
      Old : String_Access := Source.Reference;

   begin
      Source.Reference := new String'(Fixed.Trim (Old.all, Left, Right));
      Free (Old);
   end Trim;

end Ada.Strings.Unbounded;
