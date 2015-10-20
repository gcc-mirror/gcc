------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S T R I N G S . S U P E R B O U N D E D              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2015, Free Software Foundation, Inc.         --
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

with Ada.Strings.Maps;   use Ada.Strings.Maps;
with Ada.Strings.Search;

package body Ada.Strings.Superbounded is

   ------------
   -- Concat --
   ------------

   function Concat
     (Left  : Super_String;
      Right : Super_String) return Super_String
   is
   begin
      return Result : Super_String (Left.Max_Length) do
         declare
            Llen : constant Natural := Left.Current_Length;
            Rlen : constant Natural := Right.Current_Length;
            Nlen : constant Natural := Llen + Rlen;
         begin
            if Nlen > Left.Max_Length then
               raise Ada.Strings.Length_Error;
            end if;

            Result.Current_Length := Nlen;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);
         end;
      end return;
   end Concat;

   function Concat
     (Left  : Super_String;
      Right : String) return Super_String
   is
   begin
      return Result : Super_String (Left.Max_Length) do
         declare
            Llen   : constant Natural := Left.Current_Length;
            Nlen   : constant Natural := Llen + Right'Length;
         begin
            if Nlen > Left.Max_Length then
               raise Ada.Strings.Length_Error;
            end if;

            Result.Current_Length := Nlen;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1 .. Nlen) := Right;
         end;
      end return;
   end Concat;

   function Concat
     (Left  : String;
      Right : Super_String) return Super_String
   is

   begin
      return Result : Super_String (Right.Max_Length) do
         declare
            Llen : constant Natural := Left'Length;
            Rlen : constant Natural := Right.Current_Length;
            Nlen : constant Natural := Llen + Rlen;
         begin
            if Nlen > Right.Max_Length then
               raise Ada.Strings.Length_Error;
            end if;

            Result.Current_Length := Nlen;
            Result.Data (1 .. Llen) := Left;
            Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);
         end;
      end return;
   end Concat;

   function Concat
     (Left  : Super_String;
      Right : Character) return Super_String
   is
   begin
      return Result : Super_String (Left.Max_Length) do
         declare
            Llen : constant Natural := Left.Current_Length;
         begin
            if Llen = Left.Max_Length then
               raise Ada.Strings.Length_Error;
            end if;

            Result.Current_Length := Llen + 1;
            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Result.Current_Length) := Right;
         end;
      end return;
   end Concat;

   function Concat
     (Left  : Character;
      Right : Super_String) return Super_String
   is
   begin
      return Result : Super_String (Right.Max_Length) do
         declare
            Rlen : constant Natural := Right.Current_Length;
         begin
            if Rlen = Right.Max_Length then
               raise Ada.Strings.Length_Error;
            end if;

            Result.Current_Length := Rlen + 1;
            Result.Data (1) := Left;
            Result.Data (2 .. Result.Current_Length) :=
              Right.Data (1 .. Rlen);
         end;
      end return;
   end Concat;

   -----------
   -- Equal --
   -----------

   function "="
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Left.Current_Length = Right.Current_Length
        and then Left.Data (1 .. Left.Current_Length) =
                   Right.Data (1 .. Right.Current_Length);
   end "=";

   function Equal
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Left.Current_Length = Right'Length
        and then Left.Data (1 .. Left.Current_Length) = Right;
   end Equal;

   function Equal
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left'Length = Right.Current_Length
        and then Left = Right.Data (1 .. Right.Current_Length);
   end Equal;

   -------------
   -- Greater --
   -------------

   function Greater
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) >
               Right.Data (1 .. Right.Current_Length);
   end Greater;

   function Greater
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) > Right;
   end Greater;

   function Greater
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left > Right.Data (1 .. Right.Current_Length);
   end Greater;

   ----------------------
   -- Greater_Or_Equal --
   ----------------------

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) >=
               Right.Data (1 .. Right.Current_Length);
   end Greater_Or_Equal;

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) >= Right;
   end Greater_Or_Equal;

   function Greater_Or_Equal
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left >= Right.Data (1 .. Right.Current_Length);
   end Greater_Or_Equal;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) <
               Right.Data (1 .. Right.Current_Length);
   end Less;

   function Less
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) < Right;
   end Less;

   function Less
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left < Right.Data (1 .. Right.Current_Length);
   end Less;

   -------------------
   -- Less_Or_Equal --
   -------------------

   function Less_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) <=
               Right.Data (1 .. Right.Current_Length);
   end Less_Or_Equal;

   function Less_Or_Equal
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Left.Data (1 .. Left.Current_Length) <= Right;
   end Less_Or_Equal;

   function Less_Or_Equal
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left <= Right.Data (1 .. Right.Current_Length);
   end Less_Or_Equal;

   ----------------------
   -- Set_Super_String --
   ----------------------

   procedure Set_Super_String
     (Target : out Super_String;
      Source : String;
      Drop   : Truncation := Error)
   is
      Slen       : constant Natural := Source'Length;
      Max_Length : constant Positive := Target.Max_Length;

   begin
      if Slen <= Max_Length then
         Target.Current_Length := Slen;
         Target.Data (1 .. Slen) := Source;

      else
         case Drop is
            when Strings.Right =>
               Target.Current_Length := Max_Length;
               Target.Data (1 .. Max_Length) :=
                 Source (Source'First .. Source'First - 1 + Max_Length);

            when Strings.Left =>
               Target.Current_Length := Max_Length;
               Target.Data (1 .. Max_Length) :=
                 Source (Source'Last - (Max_Length - 1) .. Source'Last);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Set_Super_String;

   ------------------
   -- Super_Append --
   ------------------

   --  Case of Super_String and Super_String

   function Super_Append
     (Left  : Super_String;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String
   is
      Max_Length : constant Positive := Left.Max_Length;
      Result : Super_String (Max_Length);
      Llen   : constant Natural := Left.Current_Length;
      Rlen   : constant Natural := Right.Current_Length;
      Nlen   : constant Natural := Llen + Rlen;

   begin
      if Nlen <= Max_Length then
         Result.Current_Length := Nlen;
         Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
         Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Llen >= Max_Length then -- only case is Llen = Max_Length
                  Result.Data := Left.Data;

               else
                  Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
                  Result.Data (Llen + 1 .. Max_Length) :=
                    Right.Data (1 .. Max_Length - Llen);
               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then -- only case is Rlen = Max_Length
                  Result.Data := Right.Data;

               else
                  Result.Data (1 .. Max_Length - Rlen) :=
                    Left.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                  Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    Right.Data (1 .. Rlen);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Append;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Super_String;
      Drop     : Truncation := Error)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Llen       : constant Natural := Source.Current_Length;
      Rlen       : constant Natural := New_Item.Current_Length;
      Nlen       : constant Natural := Llen + Rlen;

   begin
      if Nlen <= Max_Length then
         Source.Current_Length := Nlen;
         Source.Data (Llen + 1 .. Nlen) := New_Item.Data (1 .. Rlen);

      else
         Source.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Llen < Max_Length then
                  Source.Data (Llen + 1 .. Max_Length) :=
                    New_Item.Data (1 .. Max_Length - Llen);
               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then -- only case is Rlen = Max_Length
                  Source.Data := New_Item.Data;

               else
                  Source.Data (1 .. Max_Length - Rlen) :=
                    Source.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                  Source.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    New_Item.Data (1 .. Rlen);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

   end Super_Append;

   --  Case of Super_String and String

   function Super_Append
     (Left  : Super_String;
      Right : String;
      Drop  : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Left.Max_Length;
      Result : Super_String (Max_Length);
      Llen   : constant Natural := Left.Current_Length;
      Rlen   : constant Natural := Right'Length;
      Nlen   : constant Natural := Llen + Rlen;

   begin
      if Nlen <= Max_Length then
         Result.Current_Length := Nlen;
         Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
         Result.Data (Llen + 1 .. Nlen) := Right;

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Llen >= Max_Length then -- only case is Llen = Max_Length
                  Result.Data := Left.Data;

               else
                  Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
                  Result.Data (Llen + 1 .. Max_Length) :=
                    Right (Right'First .. Right'First - 1 +
                             Max_Length - Llen);

               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then
                  Result.Data (1 .. Max_Length) :=
                    Right (Right'Last - (Max_Length - 1) .. Right'Last);

               else
                  Result.Data (1 .. Max_Length - Rlen) :=
                    Left.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                  Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    Right;
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Append;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : String;
      Drop     : Truncation := Error)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Llen   : constant Natural := Source.Current_Length;
      Rlen   : constant Natural := New_Item'Length;
      Nlen   : constant Natural := Llen + Rlen;

   begin
      if Nlen <= Max_Length then
         Source.Current_Length := Nlen;
         Source.Data (Llen + 1 .. Nlen) := New_Item;

      else
         Source.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Llen < Max_Length then
                  Source.Data (Llen + 1 .. Max_Length) :=
                    New_Item (New_Item'First ..
                                New_Item'First - 1 + Max_Length - Llen);
               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then
                  Source.Data (1 .. Max_Length) :=
                    New_Item (New_Item'Last - (Max_Length - 1) ..
                                New_Item'Last);

               else
                  Source.Data (1 .. Max_Length - Rlen) :=
                    Source.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                  Source.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    New_Item;
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

   end Super_Append;

   --  Case of String and Super_String

   function Super_Append
     (Left  : String;
      Right : Super_String;
      Drop  : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Right.Max_Length;
      Result     : Super_String (Max_Length);
      Llen       : constant Natural := Left'Length;
      Rlen       : constant Natural := Right.Current_Length;
      Nlen       : constant Natural := Llen + Rlen;

   begin
      if Nlen <= Max_Length then
         Result.Current_Length := Nlen;
         Result.Data (1 .. Llen) := Left;
         Result.Data (Llen + 1 .. Llen + Rlen) := Right.Data (1 .. Rlen);

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Llen >= Max_Length then
                  Result.Data (1 .. Max_Length) :=
                    Left (Left'First .. Left'First + (Max_Length - 1));

               else
                  Result.Data (1 .. Llen) := Left;
                  Result.Data (Llen + 1 .. Max_Length) :=
                    Right.Data (1 .. Max_Length - Llen);
               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then
                  Result.Data (1 .. Max_Length) :=
                    Right.Data (Rlen - (Max_Length - 1) .. Rlen);

               else
                  Result.Data (1 .. Max_Length - Rlen) :=
                    Left (Left'Last - (Max_Length - Rlen - 1) .. Left'Last);
                  Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    Right.Data (1 .. Rlen);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Append;

   --  Case of Super_String and Character

   function Super_Append
     (Left  : Super_String;
      Right : Character;
      Drop  : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Left.Max_Length;
      Result     : Super_String (Max_Length);
      Llen       : constant Natural := Left.Current_Length;

   begin
      if Llen  < Max_Length then
         Result.Current_Length := Llen + 1;
         Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
         Result.Data (Llen + 1) := Right;
         return Result;

      else
         case Drop is
            when Strings.Right =>
               return Left;

            when Strings.Left =>
               Result.Current_Length := Max_Length;
               Result.Data (1 .. Max_Length - 1) :=
                 Left.Data (2 .. Max_Length);
               Result.Data (Max_Length) := Right;
               return Result;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Super_Append;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Character;
      Drop     : Truncation := Error)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Llen       : constant Natural  := Source.Current_Length;

   begin
      if Llen  < Max_Length then
         Source.Current_Length := Llen + 1;
         Source.Data (Llen + 1) := New_Item;

      else
         Source.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               null;

            when Strings.Left =>
               Source.Data (1 .. Max_Length - 1) :=
                 Source.Data (2 .. Max_Length);
               Source.Data (Max_Length) := New_Item;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

   end Super_Append;

   --  Case of Character and Super_String

   function Super_Append
     (Left  : Character;
      Right : Super_String;
      Drop  : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Right.Max_Length;
      Result : Super_String (Max_Length);
      Rlen   : constant Natural := Right.Current_Length;

   begin
      if Rlen < Max_Length then
         Result.Current_Length := Rlen + 1;
         Result.Data (1) := Left;
         Result.Data (2 .. Rlen + 1) := Right.Data (1 .. Rlen);
         return Result;

      else
         case Drop is
            when Strings.Right =>
               Result.Current_Length := Max_Length;
               Result.Data (1) := Left;
               Result.Data (2 .. Max_Length) :=
                 Right.Data (1 .. Max_Length - 1);
               return Result;

            when Strings.Left =>
               return Right;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Super_Append;

   -----------------
   -- Super_Count --
   -----------------

   function Super_Count
     (Source  : Super_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      return
        Search.Count
          (Source.Data (1 .. Source.Current_Length), Pattern, Mapping);
   end Super_Count;

   function Super_Count
     (Source  : Super_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return
        Search.Count
          (Source.Data (1 .. Source.Current_Length), Pattern, Mapping);
   end Super_Count;

   function Super_Count
     (Source : Super_String;
      Set    : Maps.Character_Set) return Natural
   is
   begin
      return Search.Count (Source.Data (1 .. Source.Current_Length), Set);
   end Super_Count;

   ------------------
   -- Super_Delete --
   ------------------

   function Super_Delete
     (Source  : Super_String;
      From    : Positive;
      Through : Natural) return Super_String
   is
      Result     : Super_String (Source.Max_Length);
      Slen       : constant Natural := Source.Current_Length;
      Num_Delete : constant Integer := Through - From + 1;

   begin
      if Num_Delete <= 0 then
         return Source;

      elsif From > Slen + 1 then
         raise Ada.Strings.Index_Error;

      elsif Through >= Slen then
         Result.Current_Length := From - 1;
         Result.Data (1 .. From - 1) := Source.Data (1 .. From - 1);
         return Result;

      else
         Result.Current_Length := Slen - Num_Delete;
         Result.Data (1 .. From - 1) := Source.Data (1 .. From - 1);
         Result.Data (From .. Result.Current_Length) :=
           Source.Data (Through + 1 .. Slen);
         return Result;
      end if;
   end Super_Delete;

   procedure Super_Delete
     (Source  : in out Super_String;
      From    : Positive;
      Through : Natural)
   is
      Slen       : constant Natural := Source.Current_Length;
      Num_Delete : constant Integer := Through - From + 1;

   begin
      if Num_Delete <= 0 then
         return;

      elsif From > Slen + 1 then
         raise Ada.Strings.Index_Error;

      elsif Through >= Slen then
         Source.Current_Length := From - 1;

      else
         Source.Current_Length := Slen - Num_Delete;
         Source.Data (From .. Source.Current_Length) :=
           Source.Data (Through + 1 .. Slen);
      end if;
   end Super_Delete;

   -------------------
   -- Super_Element --
   -------------------

   function Super_Element
     (Source : Super_String;
      Index  : Positive) return Character
   is
   begin
      if Index <= Source.Current_Length then
         return Source.Data (Index);
      else
         raise Strings.Index_Error;
      end if;
   end Super_Element;

   ----------------------
   -- Super_Find_Token --
   ----------------------

   procedure Super_Find_Token
     (Source : Super_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Search.Find_Token
        (Source.Data (From .. Source.Current_Length), Set, Test, First, Last);
   end Super_Find_Token;

   procedure Super_Find_Token
     (Source : Super_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Search.Find_Token
        (Source.Data (1 .. Source.Current_Length), Set, Test, First, Last);
   end Super_Find_Token;

   ----------------
   -- Super_Head --
   ----------------

   function Super_Head
     (Source : Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Source.Max_Length;
      Result     : Super_String (Max_Length);
      Slen       : constant Natural := Source.Current_Length;
      Npad       : constant Integer := Count - Slen;

   begin
      if Npad <= 0 then
         Result.Current_Length := Count;
         Result.Data (1 .. Count) := Source.Data (1 .. Count);

      elsif Count <= Max_Length then
         Result.Current_Length := Count;
         Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
         Result.Data (Slen + 1 .. Count) := (others => Pad);

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
               Result.Data (Slen + 1 .. Max_Length) := (others => Pad);

            when Strings.Left =>
               if Npad >= Max_Length then
                  Result.Data := (others => Pad);

               else
                  Result.Data (1 .. Max_Length - Npad) :=
                    Source.Data (Count - Max_Length + 1 .. Slen);
                  Result.Data (Max_Length - Npad + 1 .. Max_Length) :=
                    (others => Pad);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Head;

   procedure Super_Head
     (Source : in out Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Slen       : constant Natural  := Source.Current_Length;
      Npad       : constant Integer  := Count - Slen;
      Temp       : String (1 .. Max_Length);

   begin
      if Npad <= 0 then
         Source.Current_Length := Count;

      elsif Count <= Max_Length then
         Source.Current_Length := Count;
         Source.Data (Slen + 1 .. Count) := (others => Pad);

      else
         Source.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               Source.Data (Slen + 1 .. Max_Length) := (others => Pad);

            when Strings.Left =>
               if Npad > Max_Length then
                  Source.Data := (others => Pad);

               else
                  Temp := Source.Data;
                  Source.Data (1 .. Max_Length - Npad) :=
                    Temp (Count - Max_Length + 1 .. Slen);

                  for J in Max_Length - Npad + 1 .. Max_Length loop
                     Source.Data (J) := Pad;
                  end loop;
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Super_Head;

   -----------------
   -- Super_Index --
   -----------------

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Strings.Direction := Strings.Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      return Search.Index
        (Source.Data (1 .. Source.Current_Length), Pattern, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return Search.Index
        (Source.Data (1 .. Source.Current_Length), Pattern, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source : Super_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward) return Natural
   is
   begin
      return Search.Index
        (Source.Data (1 .. Source.Current_Length), Set, Test, Going);
   end Super_Index;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   is
   begin
      return Search.Index
        (Source.Data (1 .. Source.Current_Length),
         Pattern, From, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return Search.Index
        (Source.Data (1 .. Source.Current_Length),
         Pattern, From, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source : Super_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      return Search.Index
        (Source.Data (1 .. Source.Current_Length), Set, From, Test, Going);
   end Super_Index;

   ---------------------------
   -- Super_Index_Non_Blank --
   ---------------------------

   function Super_Index_Non_Blank
     (Source : Super_String;
      Going  : Strings.Direction := Strings.Forward) return Natural
   is
   begin
      return
        Search.Index_Non_Blank
          (Source.Data (1 .. Source.Current_Length), Going);
   end Super_Index_Non_Blank;

   function Super_Index_Non_Blank
     (Source : Super_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
      return
        Search.Index_Non_Blank
          (Source.Data (1 .. Source.Current_Length), From, Going);
   end Super_Index_Non_Blank;

   ------------------
   -- Super_Insert --
   ------------------

   function Super_Insert
     (Source   : Super_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Source.Max_Length;
      Result     : Super_String (Max_Length);
      Slen       : constant Natural := Source.Current_Length;
      Nlen       : constant Natural := New_Item'Length;
      Tlen       : constant Natural := Slen + Nlen;
      Blen       : constant Natural := Before - 1;
      Alen       : constant Integer := Slen - Blen;
      Droplen    : constant Integer := Tlen - Max_Length;

      --  Tlen is the length of the total string before possible truncation.
      --  Blen, Alen are the lengths of the before and after pieces of the
      --  source string.

   begin
      if Alen < 0 then
         raise Ada.Strings.Index_Error;

      elsif Droplen <= 0 then
         Result.Current_Length := Tlen;
         Result.Data (1 .. Blen) := Source.Data (1 .. Blen);
         Result.Data (Before .. Before + Nlen - 1) := New_Item;
         Result.Data (Before + Nlen .. Tlen) :=
           Source.Data (Before .. Slen);

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Blen) := Source.Data (1 .. Blen);

               if Droplen > Alen then
                  Result.Data (Before .. Max_Length) :=
                    New_Item (New_Item'First
                                .. New_Item'First + Max_Length - Before);
               else
                  Result.Data (Before .. Before + Nlen - 1) := New_Item;
                  Result.Data (Before + Nlen .. Max_Length) :=
                    Source.Data (Before .. Slen - Droplen);
               end if;

            when Strings.Left =>
               Result.Data (Max_Length - (Alen - 1) .. Max_Length) :=
                 Source.Data (Before .. Slen);

               if Droplen >= Blen then
                  Result.Data (1 .. Max_Length - Alen) :=
                    New_Item (New_Item'Last - (Max_Length - Alen) + 1
                                .. New_Item'Last);
               else
                  Result.Data
                    (Blen - Droplen + 1 .. Max_Length - Alen) :=
                    New_Item;
                  Result.Data (1 .. Blen - Droplen) :=
                    Source.Data (Droplen + 1 .. Blen);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Insert;

   procedure Super_Insert
     (Source   : in out Super_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Strings.Truncation := Strings.Error)
   is
   begin
      --  We do a double copy here because this is one of the situations
      --  in which we move data to the right, and at least at the moment,
      --  GNAT is not handling such cases correctly ???

      Source := Super_Insert (Source, Before, New_Item, Drop);
   end Super_Insert;

   ------------------
   -- Super_Length --
   ------------------

   function Super_Length (Source : Super_String) return Natural is
   begin
      return Source.Current_Length;
   end Super_Length;

   ---------------------
   -- Super_Overwrite --
   ---------------------

   function Super_Overwrite
     (Source   : Super_String;
      Position : Positive;
      New_Item : String;
      Drop     : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Source.Max_Length;
      Result     : Super_String (Max_Length);
      Endpos     : constant Natural  := Position + New_Item'Length - 1;
      Slen       : constant Natural  := Source.Current_Length;
      Droplen    : Natural;

   begin
      if Position > Slen + 1 then
         raise Ada.Strings.Index_Error;

      elsif New_Item'Length = 0 then
         return Source;

      elsif Endpos <= Slen then
         Result.Current_Length := Source.Current_Length;
         Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
         Result.Data (Position .. Endpos) := New_Item;
         return Result;

      elsif Endpos <= Max_Length then
         Result.Current_Length := Endpos;
         Result.Data (1 .. Position - 1) := Source.Data (1 .. Position - 1);
         Result.Data (Position .. Endpos) := New_Item;
         return Result;

      else
         Result.Current_Length := Max_Length;
         Droplen := Endpos - Max_Length;

         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Position - 1) :=
                 Source.Data (1 .. Position - 1);

               Result.Data (Position .. Max_Length) :=
                 New_Item (New_Item'First .. New_Item'Last - Droplen);
               return Result;

            when Strings.Left =>
               if New_Item'Length >= Max_Length then
                  Result.Data (1 .. Max_Length) :=
                    New_Item (New_Item'Last - Max_Length + 1 ..
                                New_Item'Last);
                  return Result;

               else
                  Result.Data (1 .. Max_Length - New_Item'Length) :=
                    Source.Data (Droplen + 1 .. Position - 1);
                  Result.Data
                    (Max_Length - New_Item'Length + 1 .. Max_Length) :=
                    New_Item;
                  return Result;
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Super_Overwrite;

   procedure Super_Overwrite
     (Source    : in out Super_String;
      Position  : Positive;
      New_Item  : String;
      Drop      : Strings.Truncation := Strings.Error)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Endpos     : constant Positive := Position + New_Item'Length - 1;
      Slen       : constant Natural  := Source.Current_Length;
      Droplen    : Natural;

   begin
      if Position > Slen + 1 then
         raise Ada.Strings.Index_Error;

      elsif Endpos <= Slen then
         Source.Data (Position .. Endpos) := New_Item;

      elsif Endpos <= Max_Length then
         Source.Data (Position .. Endpos) := New_Item;
         Source.Current_Length := Endpos;

      else
         Source.Current_Length := Max_Length;
         Droplen := Endpos - Max_Length;

         case Drop is
            when Strings.Right =>
               Source.Data (Position .. Max_Length) :=
                 New_Item (New_Item'First .. New_Item'Last - Droplen);

            when Strings.Left =>
               if New_Item'Length > Max_Length then
                  Source.Data (1 .. Max_Length) :=
                    New_Item (New_Item'Last - Max_Length + 1 ..
                                New_Item'Last);

               else
                  Source.Data (1 .. Max_Length - New_Item'Length) :=
                    Source.Data (Droplen + 1 .. Position - 1);

                  Source.Data
                    (Max_Length - New_Item'Length + 1 .. Max_Length) :=
                    New_Item;
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Super_Overwrite;

   ---------------------------
   -- Super_Replace_Element --
   ---------------------------

   procedure Super_Replace_Element
     (Source : in out Super_String;
      Index  : Positive;
      By     : Character)
   is
   begin
      if Index <= Source.Current_Length then
         Source.Data (Index) := By;
      else
         raise Ada.Strings.Index_Error;
      end if;
   end Super_Replace_Element;

   -------------------------
   -- Super_Replace_Slice --
   -------------------------

   function Super_Replace_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural;
      By     : String;
      Drop   : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Source.Max_Length;
      Slen       : constant Natural  := Source.Current_Length;

   begin
      if Low > Slen + 1 then
         raise Strings.Index_Error;

      elsif High < Low then
         return Super_Insert (Source, Low, By, Drop);

      else
         declare
            Blen    : constant Natural := Natural'Max (0, Low - 1);
            Alen    : constant Natural := Natural'Max (0, Slen - High);
            Tlen    : constant Natural := Blen + By'Length + Alen;
            Droplen : constant Integer := Tlen - Max_Length;
            Result  : Super_String (Max_Length);

            --  Tlen is the total length of the result string before any
            --  truncation. Blen and Alen are the lengths of the pieces
            --  of the original string that end up in the result string
            --  before and after the replaced slice.

         begin
            if Droplen <= 0 then
               Result.Current_Length := Tlen;
               Result.Data (1 .. Blen) := Source.Data (1 .. Blen);
               Result.Data (Low .. Low + By'Length - 1) := By;
               Result.Data (Low + By'Length .. Tlen) :=
                 Source.Data (High + 1 .. Slen);

            else
               Result.Current_Length := Max_Length;

               case Drop is
                  when Strings.Right =>
                     Result.Data (1 .. Blen) := Source.Data (1 .. Blen);

                     if Droplen > Alen then
                        Result.Data (Low .. Max_Length) :=
                          By (By'First .. By'First + Max_Length - Low);
                     else
                        Result.Data (Low .. Low + By'Length - 1) := By;
                        Result.Data (Low + By'Length .. Max_Length) :=
                          Source.Data (High + 1 .. Slen - Droplen);
                     end if;

                  when Strings.Left =>
                     Result.Data (Max_Length - (Alen - 1) .. Max_Length) :=
                       Source.Data (High + 1 .. Slen);

                     if Droplen >= Blen then
                        Result.Data (1 .. Max_Length - Alen) :=
                          By (By'Last - (Max_Length - Alen) + 1 .. By'Last);
                     else
                        Result.Data
                          (Blen - Droplen + 1 .. Max_Length - Alen) := By;
                        Result.Data (1 .. Blen - Droplen) :=
                          Source.Data (Droplen + 1 .. Blen);
                     end if;

                  when Strings.Error =>
                     raise Ada.Strings.Length_Error;
               end case;
            end if;

            return Result;
         end;
      end if;
   end Super_Replace_Slice;

   procedure Super_Replace_Slice
     (Source   : in out Super_String;
      Low      : Positive;
      High     : Natural;
      By       : String;
      Drop     : Strings.Truncation := Strings.Error)
   is
   begin
      --  We do a double copy here because this is one of the situations
      --  in which we move data to the right, and at least at the moment,
      --  GNAT is not handling such cases correctly ???

      Source := Super_Replace_Slice (Source, Low, High, By, Drop);
   end Super_Replace_Slice;

   ---------------------
   -- Super_Replicate --
   ---------------------

   function Super_Replicate
     (Count      : Natural;
      Item       : Character;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String
   is
      Result : Super_String (Max_Length);

   begin
      if Count <= Max_Length then
         Result.Current_Length := Count;

      elsif Drop = Strings.Error then
         raise Ada.Strings.Length_Error;

      else
         Result.Current_Length := Max_Length;
      end if;

      Result.Data (1 .. Result.Current_Length) := (others => Item);
      return Result;
   end Super_Replicate;

   function Super_Replicate
     (Count      : Natural;
      Item       : String;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String
   is
      Length : constant Integer := Count * Item'Length;
      Result : Super_String (Max_Length);
      Indx   : Positive;

   begin
      if Length <= Max_Length then
         Result.Current_Length := Length;

         if Length > 0 then
            Indx := 1;

            for J in 1 .. Count loop
               Result.Data (Indx .. Indx + Item'Length - 1) := Item;
               Indx := Indx + Item'Length;
            end loop;
         end if;

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               Indx := 1;

               while Indx + Item'Length <= Max_Length + 1 loop
                  Result.Data (Indx .. Indx + Item'Length - 1) := Item;
                  Indx := Indx + Item'Length;
               end loop;

               Result.Data (Indx .. Max_Length) :=
                 Item (Item'First .. Item'First + Max_Length - Indx);

            when Strings.Left =>
               Indx := Max_Length;

               while Indx - Item'Length >= 1 loop
                  Result.Data (Indx - (Item'Length - 1) .. Indx) := Item;
                  Indx := Indx - Item'Length;
               end loop;

               Result.Data (1 .. Indx) :=
                 Item (Item'Last - Indx + 1 .. Item'Last);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Replicate;

   function Super_Replicate
     (Count : Natural;
      Item  : Super_String;
      Drop  : Strings.Truncation := Strings.Error) return Super_String
   is
   begin
      return
        Super_Replicate
          (Count,
           Item.Data (1 .. Item.Current_Length),
           Drop,
           Item.Max_Length);
   end Super_Replicate;

   -----------------
   -- Super_Slice --
   -----------------

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return String
   is
   begin
      --  Note: test of High > Length is in accordance with AI95-00128

      return R : String (Low .. High) do
         if Low > Source.Current_Length + 1
           or else High > Source.Current_Length
         then
            raise Index_Error;
         end if;

         --  Note: in this case, superflat bounds are not a problem, we just
         --  get the null string in accordance with normal Ada slice rules.

         R := Source.Data (Low .. High);
      end return;
   end Super_Slice;

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return Super_String
   is
   begin
      return Result : Super_String (Source.Max_Length) do
         if Low > Source.Current_Length + 1
           or else High > Source.Current_Length
         then
            raise Index_Error;
         end if;

         --  Note: the Max operation here deals with the superflat case

         Result.Current_Length := Integer'Max (0, High - Low + 1);
         Result.Data (1 .. Result.Current_Length) := Source.Data (Low .. High);
      end return;
   end Super_Slice;

   procedure Super_Slice
     (Source : Super_String;
      Target : out Super_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      if Low > Source.Current_Length + 1
        or else High > Source.Current_Length
      then
         raise Index_Error;
      end if;

      --  Note: the Max operation here deals with the superflat case

      Target.Current_Length := Integer'Max (0, High - Low + 1);
      Target.Data (1 .. Target.Current_Length) := Source.Data (Low .. High);
   end Super_Slice;

   ----------------
   -- Super_Tail --
   ----------------

   function Super_Tail
     (Source : Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Strings.Truncation := Strings.Error) return Super_String
   is
      Max_Length : constant Positive := Source.Max_Length;
      Result     : Super_String (Max_Length);
      Slen       : constant Natural := Source.Current_Length;
      Npad       : constant Integer := Count - Slen;

   begin
      if Npad <= 0 then
         Result.Current_Length := Count;
         Result.Data (1 .. Count) :=
           Source.Data (Slen - (Count - 1) .. Slen);

      elsif Count <= Max_Length then
         Result.Current_Length := Count;
         Result.Data (1 .. Npad) := (others => Pad);
         Result.Data (Npad + 1 .. Count) := Source.Data (1 .. Slen);

      else
         Result.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Npad >= Max_Length then
                  Result.Data := (others => Pad);

               else
                  Result.Data (1 .. Npad) := (others => Pad);
                  Result.Data (Npad + 1 .. Max_Length) :=
                    Source.Data (1 .. Max_Length - Npad);
               end if;

            when Strings.Left =>
               Result.Data (1 .. Max_Length - Slen) := (others => Pad);
               Result.Data (Max_Length - Slen + 1 .. Max_Length) :=
                 Source.Data (1 .. Slen);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end Super_Tail;

   procedure Super_Tail
     (Source : in out Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Slen       : constant Natural  := Source.Current_Length;
      Npad       : constant Integer  := Count - Slen;

      Temp : constant String (1 .. Max_Length) := Source.Data;

   begin
      if Npad <= 0 then
         Source.Current_Length := Count;
         Source.Data (1 .. Count) :=
           Temp (Slen - (Count - 1) .. Slen);

      elsif Count <= Max_Length then
         Source.Current_Length := Count;
         Source.Data (1 .. Npad) := (others => Pad);
         Source.Data (Npad + 1 .. Count) := Temp (1 .. Slen);

      else
         Source.Current_Length := Max_Length;

         case Drop is
            when Strings.Right =>
               if Npad >= Max_Length then
                  Source.Data := (others => Pad);

               else
                  Source.Data (1 .. Npad) := (others => Pad);
                  Source.Data (Npad + 1 .. Max_Length) :=
                    Temp (1 .. Max_Length - Npad);
               end if;

            when Strings.Left =>
               for J in 1 .. Max_Length - Slen loop
                  Source.Data (J) := Pad;
               end loop;

               Source.Data (Max_Length - Slen + 1 .. Max_Length) :=
                 Temp (1 .. Slen);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;
   end Super_Tail;

   ---------------------
   -- Super_To_String --
   ---------------------

   function Super_To_String (Source : Super_String) return String is
   begin
      return R : String (1 .. Source.Current_Length) do
         R := Source.Data (1 .. Source.Current_Length);
      end return;
   end Super_To_String;

   ---------------------
   -- Super_Translate --
   ---------------------

   function Super_Translate
     (Source  : Super_String;
      Mapping : Maps.Character_Mapping) return Super_String
   is
      Result : Super_String (Source.Max_Length);

   begin
      Result.Current_Length := Source.Current_Length;

      for J in 1 .. Source.Current_Length loop
         Result.Data (J) := Value (Mapping, Source.Data (J));
      end loop;

      return Result;
   end Super_Translate;

   procedure Super_Translate
     (Source  : in out Super_String;
      Mapping : Maps.Character_Mapping)
   is
   begin
      for J in 1 .. Source.Current_Length loop
         Source.Data (J) := Value (Mapping, Source.Data (J));
      end loop;
   end Super_Translate;

   function Super_Translate
     (Source  : Super_String;
      Mapping : Maps.Character_Mapping_Function) return Super_String
   is
      Result : Super_String (Source.Max_Length);

   begin
      Result.Current_Length := Source.Current_Length;

      for J in 1 .. Source.Current_Length loop
         Result.Data (J) := Mapping.all (Source.Data (J));
      end loop;

      return Result;
   end Super_Translate;

   procedure Super_Translate
     (Source  : in out Super_String;
      Mapping : Maps.Character_Mapping_Function)
   is
   begin
      for J in 1 .. Source.Current_Length loop
         Source.Data (J) := Mapping.all (Source.Data (J));
      end loop;
   end Super_Translate;

   ----------------
   -- Super_Trim --
   ----------------

   function Super_Trim
     (Source : Super_String;
      Side   : Trim_End) return Super_String
   is
      Result : Super_String (Source.Max_Length);
      Last   : Natural := Source.Current_Length;
      First  : Positive := 1;

   begin
      if Side = Left or else Side = Both then
         while First <= Last and then Source.Data (First) = ' ' loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or else Side = Both then
         while Last >= First and then Source.Data (Last) = ' ' loop
            Last := Last - 1;
         end loop;
      end if;

      Result.Current_Length := Last - First + 1;
      Result.Data (1 .. Result.Current_Length) := Source.Data (First .. Last);
      return Result;
   end Super_Trim;

   procedure Super_Trim
     (Source : in out Super_String;
      Side   : Trim_End)
   is
      Max_Length : constant Positive := Source.Max_Length;
      Last       : Natural           := Source.Current_Length;
      First      : Positive          := 1;
      Temp       : String (1 .. Max_Length);

   begin
      Temp (1 .. Last) := Source.Data (1 .. Last);

      if Side = Left or else Side = Both then
         while First <= Last and then Temp (First) = ' ' loop
            First := First + 1;
         end loop;
      end if;

      if Side = Right or else Side = Both then
         while Last >= First and then Temp (Last) = ' ' loop
            Last := Last - 1;
         end loop;
      end if;

      Source.Current_Length := Last - First + 1;
      Source.Data (1 .. Source.Current_Length) := Temp (First .. Last);
   end Super_Trim;

   function Super_Trim
     (Source : Super_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Super_String
   is
      Result : Super_String (Source.Max_Length);

   begin
      for First in 1 .. Source.Current_Length loop
         if not Is_In (Source.Data (First), Left) then
            for Last in reverse First .. Source.Current_Length loop
               if not Is_In (Source.Data (Last), Right) then
                  Result.Current_Length := Last - First + 1;
                  Result.Data (1 .. Result.Current_Length) :=
                    Source.Data (First .. Last);
                  return Result;
               end if;
            end loop;
         end if;
      end loop;

      Result.Current_Length := 0;
      return Result;
   end Super_Trim;

   procedure Super_Trim
     (Source : in out Super_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set)
   is
   begin
      for First in 1 .. Source.Current_Length loop
         if not Is_In (Source.Data (First), Left) then
            for Last in reverse First .. Source.Current_Length loop
               if not Is_In (Source.Data (Last), Right) then
                  if First = 1 then
                     Source.Current_Length := Last;
                     return;
                  else
                     Source.Current_Length := Last - First + 1;
                     Source.Data (1 .. Source.Current_Length) :=
                       Source.Data (First .. Last);
                     return;
                  end if;
               end if;
            end loop;

            Source.Current_Length := 0;
            return;
         end if;
      end loop;

      Source.Current_Length := 0;
   end Super_Trim;

   -----------
   -- Times --
   -----------

   function Times
     (Left       : Natural;
      Right      : Character;
      Max_Length : Positive) return Super_String
   is
      Result : Super_String (Max_Length);

   begin
      if Left > Max_Length then
         raise Ada.Strings.Length_Error;

      else
         Result.Current_Length := Left;

         for J in 1 .. Left loop
            Result.Data (J) := Right;
         end loop;
      end if;

      return Result;
   end Times;

   function Times
     (Left       : Natural;
      Right      : String;
      Max_Length : Positive) return Super_String
   is
      Result : Super_String (Max_Length);
      Pos    : Positive         := 1;
      Rlen   : constant Natural := Right'Length;
      Nlen   : constant Natural := Left * Rlen;

   begin
      if Nlen > Max_Length then
         raise Ada.Strings.Length_Error;

      else
         Result.Current_Length := Nlen;

         if Nlen > 0 then
            for J in 1 .. Left loop
               Result.Data (Pos .. Pos + Rlen - 1) := Right;
               Pos := Pos + Rlen;
            end loop;
         end if;
      end if;

      return Result;
   end Times;

   function Times
     (Left  : Natural;
      Right : Super_String) return Super_String
   is
      Result : Super_String (Right.Max_Length);
      Pos    : Positive := 1;
      Rlen   : constant Natural := Right.Current_Length;
      Nlen   : constant Natural := Left * Rlen;

   begin
      if Nlen > Right.Max_Length then
         raise Ada.Strings.Length_Error;

      else
         Result.Current_Length := Nlen;

         if Nlen > 0 then
            for J in 1 .. Left loop
               Result.Data (Pos .. Pos + Rlen - 1) :=
                 Right.Data (1 .. Rlen);
               Pos := Pos + Rlen;
            end loop;
         end if;
      end if;

      return Result;
   end Times;

   ---------------------
   -- To_Super_String --
   ---------------------

   function To_Super_String
     (Source     : String;
      Max_Length : Natural;
      Drop       : Truncation := Error) return Super_String
   is
      Result : Super_String (Max_Length);
      Slen   : constant Natural := Source'Length;

   begin
      if Slen <= Max_Length then
         Result.Current_Length := Slen;
         Result.Data (1 .. Slen) := Source;

      else
         case Drop is
            when Strings.Right =>
               Result.Current_Length := Max_Length;
               Result.Data (1 .. Max_Length) :=
                 Source (Source'First .. Source'First - 1 + Max_Length);

            when Strings.Left =>
               Result.Current_Length := Max_Length;
               Result.Data (1 .. Max_Length) :=
                 Source (Source'Last - (Max_Length - 1) .. Source'Last);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end To_Super_String;

end Ada.Strings.Superbounded;
