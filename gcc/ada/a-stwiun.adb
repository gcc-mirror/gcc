------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--           A D A . S T R I N G S . W I D E _ U N B O U N D E D            --
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

with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Search;
with Ada.Unchecked_Deallocation;

package body Ada.Strings.Wide_Unbounded is

   use Ada.Finalization;

   ---------
   -- "&" --
   ---------

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String)
      return  Unbounded_Wide_String
   is
      L_Length : constant Integer := Left.Reference.all'Length;
      R_Length : constant Integer := Right.Reference.all'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Length);
      Result.Reference.all (1 .. L_Length)          := Left.Reference.all;
      Result.Reference.all (L_Length + 1 .. Length) := Right.Reference.all;
      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String)
      return  Unbounded_Wide_String
   is
      L_Length : constant Integer := Left.Reference.all'Length;
      Length   : constant Integer := L_Length +  Right'Length;
      Result   : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Length);
      Result.Reference.all (1 .. L_Length)          := Left.Reference.all;
      Result.Reference.all (L_Length + 1 .. Length) := Right;
      return Result;
   end "&";

   function "&"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String)
      return  Unbounded_Wide_String
   is
      R_Length : constant Integer := Right.Reference.all'Length;
      Length   : constant Integer := Left'Length + R_Length;
      Result   : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Length);
      Result.Reference.all (1 .. Left'Length)          := Left;
      Result.Reference.all (Left'Length + 1 .. Length) := Right.Reference.all;
      return Result;
   end "&";

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Wide_Character)
      return  Unbounded_Wide_String
   is
      Length : constant Integer := Left.Reference.all'Length + 1;
      Result : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Length);
      Result.Reference.all (1 .. Length - 1) := Left.Reference.all;
      Result.Reference.all (Length)          := Right;
      return Result;
   end "&";

   function "&"
     (Left  : Wide_Character;
      Right : Unbounded_Wide_String)
      return  Unbounded_Wide_String
   is
      Length : constant Integer      := Right.Reference.all'Length + 1;
      Result : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Length);
      Result.Reference.all (1)           := Left;
      Result.Reference.all (2 .. Length) := Right.Reference.all;
      return Result;
   end "&";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Wide_Character)
      return  Unbounded_Wide_String
   is
      Result : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Left);
      for J in Result.Reference'Range loop
         Result.Reference (J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left   : Natural;
      Right  : Wide_String)
      return   Unbounded_Wide_String
   is
      Result : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Left * Right'Length);

      for J in 1 .. Left loop
         Result.Reference.all
           (Right'Length * J - Right'Length + 1 .. Right'Length * J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_String)
      return  Unbounded_Wide_String
   is
      R_Length : constant Integer := Right.Reference.all'Length;
      Result   : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Left * R_Length);

      for I in 1 .. Left loop
         Result.Reference.all (R_Length * I - R_Length + 1 .. R_Length * I) :=
           Right.Reference.all;
      end loop;

      return Result;
   end "*";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all < Right.Reference.all;
   end "<";

   function "<"
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all < Right;
   end "<";

   function "<"
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left < Right.Reference.all;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all <= Right.Reference.all;
   end "<=";

   function "<="
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all <= Right;
   end "<=";

   function "<="
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left <= Right.Reference.all;
   end "<=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all = Right.Reference.all;
   end "=";

   function "="
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all = Right;
   end "=";

   function "="
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left = Right.Reference.all;
   end "=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all > Right.Reference.all;
   end ">";

   function ">"
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all > Right;
   end ">";

   function ">"
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left > Right.Reference.all;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all >= Right.Reference.all;
   end ">=";

   function ">="
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean
   is
   begin
      return Left.Reference.all >= Right;
   end ">=";

   function ">="
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean
   is
   begin
      return Left >= Right.Reference.all;
   end ">=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Unbounded_Wide_String) is
   begin
      --  Copy string, except we do not copy the statically allocated
      --  null string, since it can never be deallocated.

      if Object.Reference /= Null_Wide_String'Access then
         Object.Reference := new Wide_String'(Object.Reference.all);
      end if;
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : in Unbounded_Wide_String)
   is
      S_Length : constant Integer := Source.Reference.all'Length;
      Length   : constant Integer := S_Length + New_Item.Reference.all'Length;
      Temp     : Wide_String_Access := Source.Reference;

   begin
      if Source.Reference = Null_Wide_String'Access then
         Source := To_Unbounded_Wide_String (New_Item.Reference.all);
         return;
      end if;

      Source.Reference := new Wide_String (1 .. Length);

      Source.Reference.all (1 .. S_Length) := Temp.all;
      Source.Reference.all (S_Length + 1 .. Length) := New_Item.Reference.all;
      Free (Temp);
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : in Wide_String)
   is
      S_Length : constant Integer := Source.Reference.all'Length;
      Length   : constant Integer := S_Length + New_Item'Length;
      Temp     : Wide_String_Access := Source.Reference;

   begin
      if Source.Reference = Null_Wide_String'Access then
         Source := To_Unbounded_Wide_String (New_Item);
         return;
      end if;

      Source.Reference := new Wide_String (1 .. Length);
      Source.Reference.all (1 .. S_Length) := Temp.all;
      Source.Reference.all (S_Length + 1 .. Length) := New_Item;
      Free (Temp);
   end Append;

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : in Wide_Character)
   is
      S_Length : constant Integer := Source.Reference.all'Length;
      Length   : constant Integer := S_Length + 1;
      Temp     : Wide_String_Access := Source.Reference;

   begin
      if Source.Reference = Null_Wide_String'Access then
         Source := To_Unbounded_Wide_String ("" & New_Item);
         return;
      end if;

      Source.Reference := new Wide_String (1 .. Length);
      Source.Reference.all (1 .. S_Length) := Temp.all;
      Source.Reference.all (S_Length + 1) := New_Item;
      Free (Temp);
   end Append;

   -----------
   -- Count --
   -----------

   function Count
     (Source   : Unbounded_Wide_String;
      Pattern  : Wide_String;
      Mapping  : Wide_Maps.Wide_Character_Mapping :=
                        Wide_Maps.Identity)
      return     Natural
   is
   begin
      return Wide_Search.Count (Source.Reference.all, Pattern, Mapping);
   end Count;

   function Count
     (Source   : in Unbounded_Wide_String;
      Pattern  : in Wide_String;
      Mapping  : in Wide_Maps.Wide_Character_Mapping_Function)
      return     Natural
   is
   begin
      return Wide_Search.Count (Source.Reference.all, Pattern, Mapping);
   end Count;

   function Count
     (Source   : Unbounded_Wide_String;
      Set      : Wide_Maps.Wide_Character_Set)
      return     Natural
   is
   begin
      return Wide_Search.Count (Source.Reference.all, Set);
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : Unbounded_Wide_String;
      From    : Positive;
      Through : Natural)
      return    Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Delete (Source.Reference.all, From, Through));
   end Delete;

   procedure Delete
     (Source  : in out Unbounded_Wide_String;
      From    : in Positive;
      Through : in Natural)
   is
      Temp : Wide_String_Access := Source.Reference;
   begin
      Source := To_Unbounded_Wide_String
        (Wide_Fixed.Delete (Temp.all, From, Through));
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
     (Source : Unbounded_Wide_String;
      Index  : Positive)
      return   Wide_Character
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

   procedure Finalize (Object : in out Unbounded_Wide_String) is
      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Access);

   begin
      --  Note: Don't try to free statically allocated null string

      if Object.Reference /= Null_Wide_String'Access then
         Deallocate (Object.Reference);
         Object.Reference := Null_Unbounded_Wide_String.Reference;
      end if;
   end Finalize;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Wide_Search.Find_Token (Source.Reference.all, Set, Test, First, Last);
   end Find_Token;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Wide_String_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Access);
   begin
      Deallocate (X);
   end Free;

   ----------
   -- Head --
   ----------

   function Head
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space)
      return   Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Head (Source.Reference.all, Count, Pad));
   end Head;

   procedure Head
     (Source : in out Unbounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space)
   is
   begin
      Source := To_Unbounded_Wide_String
        (Wide_Fixed.Head (Source.Reference.all, Count, Pad));
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source   : Unbounded_Wide_String;
      Pattern  : Wide_String;
      Going    : Strings.Direction := Strings.Forward;
      Mapping  : Wide_Maps.Wide_Character_Mapping :=
                        Wide_Maps.Identity)
      return     Natural
   is
   begin
      return
        Wide_Search.Index (Source.Reference.all, Pattern, Going, Mapping);
   end Index;

   function Index
     (Source   : in Unbounded_Wide_String;
      Pattern  : in Wide_String;
      Going    : in Direction := Forward;
      Mapping  : in Wide_Maps.Wide_Character_Mapping_Function)
      return Natural
   is
   begin
      return
        Wide_Search.Index (Source.Reference.all, Pattern, Going, Mapping);
   end Index;

   function Index
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward)
      return   Natural
   is
   begin
      return Wide_Search.Index (Source.Reference.all, Set, Test, Going);
   end Index;

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      Going  : Strings.Direction := Strings.Forward)
      return   Natural
   is
   begin
      return Wide_Search.Index_Non_Blank (Source.Reference.all, Going);
   end Index_Non_Blank;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Unbounded_Wide_String) is
   begin
      Object.Reference := Null_Unbounded_Wide_String.Reference;
   end Initialize;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : Unbounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String)
      return     Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Insert (Source.Reference.all, Before, New_Item));
   end Insert;

   procedure Insert
     (Source   : in out Unbounded_Wide_String;
      Before   : in Positive;
      New_Item : in Wide_String)
   is
   begin
      Source := To_Unbounded_Wide_String
        (Wide_Fixed.Insert (Source.Reference.all, Before, New_Item));
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (Source : Unbounded_Wide_String) return Natural is
   begin
      return Source.Reference.all'Length;
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source    : Unbounded_Wide_String;
      Position  : Positive;
      New_Item  : Wide_String)
      return      Unbounded_Wide_String is

   begin
      return To_Unbounded_Wide_String
        (Wide_Fixed.Overwrite (Source.Reference.all, Position, New_Item));
   end Overwrite;

   procedure Overwrite
     (Source    : in out Unbounded_Wide_String;
      Position  : in Positive;
      New_Item  : in Wide_String)
   is
      Temp : Wide_String_Access := Source.Reference;
   begin
      Source := To_Unbounded_Wide_String
        (Wide_Fixed.Overwrite (Temp.all, Position, New_Item));
   end Overwrite;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out Unbounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character)
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
     (Source   : Unbounded_Wide_String;
      Low      : Positive;
      High     : Natural;
      By       : Wide_String)
      return     Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Replace_Slice (Source.Reference.all, Low, High, By));
   end Replace_Slice;

   procedure Replace_Slice
     (Source   : in out Unbounded_Wide_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Wide_String)
   is
      Temp : Wide_String_Access := Source.Reference;
   begin
      Source := To_Unbounded_Wide_String
        (Wide_Fixed.Replace_Slice (Temp.all, Low, High, By));
   end Replace_Slice;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural)
      return   Wide_String
   is
      Length : constant Natural := Source.Reference'Length;

   begin
      --  Note: test of High > Length is in accordance with AI95-00128

      if Low > Length + 1 or else High > Length then
         raise Index_Error;

      else
         declare
            Result : Wide_String (1 .. High - Low + 1);

         begin
            Result := Source.Reference.all (Low .. High);
            return Result;
         end;
      end if;
   end Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space)
      return   Unbounded_Wide_String is

   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Tail (Source.Reference.all, Count, Pad));
   end Tail;

   procedure Tail
     (Source : in out Unbounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space)
   is
      Temp : Wide_String_Access := Source.Reference;

   begin
      Source := To_Unbounded_Wide_String
        (Wide_Fixed.Tail (Temp.all, Count, Pad));
   end Tail;

   ------------------------------
   -- To_Unbounded_Wide_String --
   ------------------------------

   function To_Unbounded_Wide_String
     (Source : Wide_String)
      return   Unbounded_Wide_String
   is
      Result : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Source'Length);
      Result.Reference.all := Source;
      return Result;
   end To_Unbounded_Wide_String;

   function To_Unbounded_Wide_String (Length : in Natural)
      return Unbounded_Wide_String
   is
      Result : Unbounded_Wide_String;

   begin
      Result.Reference := new Wide_String (1 .. Length);
      return Result;
   end To_Unbounded_Wide_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String
     (Source : Unbounded_Wide_String)
      return   Wide_String
   is
   begin
      return Source.Reference.all;
   end To_Wide_String;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping)
      return    Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Translate (Source.Reference.all, Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping)
   is
   begin
      Wide_Fixed.Translate (Source.Reference.all, Mapping);
   end Translate;

   function Translate
     (Source  : in Unbounded_Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
      return    Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Translate (Source.Reference.all, Mapping));
   end Translate;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
   is
   begin
      Wide_Fixed.Translate (Source.Reference.all, Mapping);
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : in Unbounded_Wide_String;
      Side   : in Trim_End)
      return   Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Trim (Source.Reference.all, Side));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Side   : in Trim_End)
   is
      Old : Wide_String_Access := Source.Reference;
   begin
      Source.Reference := new Wide_String'(Wide_Fixed.Trim (Old.all, Side));
      Free (Old);
   end Trim;

   function Trim
     (Source : in Unbounded_Wide_String;
      Left   : in Wide_Maps.Wide_Character_Set;
      Right  : in Wide_Maps.Wide_Character_Set)
      return   Unbounded_Wide_String
   is
   begin
      return
        To_Unbounded_Wide_String
          (Wide_Fixed.Trim (Source.Reference.all, Left, Right));
   end Trim;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Left   : in Wide_Maps.Wide_Character_Set;
      Right  : in Wide_Maps.Wide_Character_Set)
   is
      Old : Wide_String_Access := Source.Reference;

   begin
      Source.Reference :=
        new Wide_String'(Wide_Fixed.Trim (Old.all, Left, Right));
      Free (Old);
   end Trim;

end Ada.Strings.Wide_Unbounded;
