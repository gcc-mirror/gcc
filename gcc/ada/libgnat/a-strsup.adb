------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S T R I N G S . S U P E R B O U N D E D              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2023, Free Software Foundation, Inc.         --
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

--  Ghost code, loop invariants and assertions in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with Ada.Strings.Maps; use Ada.Strings.Maps;

package body Ada.Strings.Superbounded with SPARK_Mode is

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

            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);

            if Rlen > 0 then
               Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);
            end if;

            Result.Current_Length := Nlen;
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

            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);

            if Right'Length > 0 then
               Result.Data (Llen + 1 .. Nlen) := Super_String_Data (Right);
            end if;

            Result.Current_Length := Nlen;
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

            Result.Data (1 .. Llen) := Super_String_Data (Left);

            if Rlen > 0 then
               Result.Data (Llen + 1 .. Nlen) := Right.Data (1 .. Rlen);
            end if;

            Result.Current_Length := Nlen;
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

            Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
            Result.Data (Llen + 1) := Right;
            Result.Current_Length := Llen + 1;
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

            Result.Data (1) := Left;
            Result.Data (2 .. Rlen + 1) := Right.Data (1 .. Rlen);
            Result.Current_Length := Rlen + 1;
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
      return Super_To_String (Left) = Super_To_String (Right);
   end "=";

   function Equal
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Super_To_String (Left) = Right;
   end Equal;

   function Equal
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left = Super_To_String (Right);
   end Equal;

   -------------
   -- Greater --
   -------------

   function Greater
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Super_To_String (Left) > Super_To_String (Right);
   end Greater;

   function Greater
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Super_To_String (Left) > Right;
   end Greater;

   function Greater
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left > Super_To_String (Right);
   end Greater;

   ----------------------
   -- Greater_Or_Equal --
   ----------------------

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Super_To_String (Left) >= Super_To_String (Right);
   end Greater_Or_Equal;

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Super_To_String (Left) >= Right;
   end Greater_Or_Equal;

   function Greater_Or_Equal
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left >= Super_To_String (Right);
   end Greater_Or_Equal;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Super_To_String (Left) < Super_To_String (Right);
   end Less;

   function Less
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Super_To_String (Left) < Right;
   end Less;

   function Less
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left < Super_To_String (Right);
   end Less;

   -------------------
   -- Less_Or_Equal --
   -------------------

   function Less_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean
   is
   begin
      return Super_To_String (Left) <= Super_To_String (Right);
   end Less_Or_Equal;

   function Less_Or_Equal
     (Left  : Super_String;
      Right : String) return Boolean
   is
   begin
      return Super_To_String (Left) <= Right;
   end Less_Or_Equal;

   function Less_Or_Equal
     (Left  : String;
      Right : Super_String) return Boolean
   is
   begin
      return Left <= Super_To_String (Right);
   end Less_Or_Equal;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (S      : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Source : Super_String) is
   begin
      String'Put_Image (S, Super_To_String (Source));
   end Put_Image;

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
         Target.Data (1 .. Slen) := Super_String_Data (Source);
         Target.Current_Length := Slen;

      else
         case Drop is
            when Strings.Right =>
               Target.Data (1 .. Max_Length) := Super_String_Data
                 (Source (Source'First .. Source'First - 1 + Max_Length));
               Target.Current_Length := Max_Length;

            when Strings.Left =>
               Target.Data (1 .. Max_Length) := Super_String_Data
                 (Source (Source'Last - (Max_Length - 1) .. Source'Last));
               Target.Current_Length := Max_Length;

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

   begin
      if Llen <= Max_Length - Rlen then
         Result.Data (1 .. Llen) := Left.Data (1 .. Llen);

         if Rlen > 0 then
            Result.Data (Llen + 1 .. Llen + Rlen) := Right.Data (1 .. Rlen);
         end if;

         Result.Current_Length := Llen + Rlen;

      else
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

         Result.Current_Length := Max_Length;
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

   begin
      if Llen <= Max_Length - Rlen then
         if Rlen > 0 then
            Source.Data (Llen + 1 .. Llen + Rlen) := New_Item.Data (1 .. Rlen);
            Source.Current_Length := Llen + Rlen;
         end if;

      else
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

         Source.Current_Length := Max_Length;
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

   begin
      if Llen <= Max_Length - Rlen then
         Result.Data (1 .. Llen) := Left.Data (1 .. Llen);

         if Rlen > 0 then
            Result.Data (Llen + 1 .. Llen + Rlen) := Super_String_Data (Right);
         end if;

         Result.Current_Length := Llen + Rlen;

      else
         case Drop is
            when Strings.Right =>
               if Llen >= Max_Length then -- only case is Llen = Max_Length
                  Result.Data := Left.Data;

               else
                  Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
                  Result.Data (Llen + 1 .. Max_Length) := Super_String_Data
                    (Right
                       (Right'First .. Right'First - 1 - Llen + Max_Length));

               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then
                  Result.Data (1 .. Max_Length) := Super_String_Data
                    (Right (Right'Last - (Max_Length - 1) .. Right'Last));

               else
                  Result.Data (1 .. Max_Length - Rlen) :=
                    Left.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                  Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    Super_String_Data (Right);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
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

   begin
      if Llen <= Max_Length - Rlen then
         if Rlen > 0 then
            Source.Data (Llen + 1 .. Llen + Rlen) :=
              Super_String_Data (New_Item);
            Source.Current_Length := Llen + Rlen;
         end if;

      else
         case Drop is
            when Strings.Right =>
               if Llen < Max_Length then
                  Source.Data (Llen + 1 .. Max_Length) := Super_String_Data
                    (New_Item (New_Item'First ..
                                New_Item'First - 1 - Llen + Max_Length));
               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then
                  Source.Data (1 .. Max_Length) := Super_String_Data
                    (New_Item (New_Item'Last - (Max_Length - 1) ..
                                New_Item'Last));

               else
                  Source.Data (1 .. Max_Length - Rlen) :=
                    Source.Data (Llen - (Max_Length - Rlen - 1) .. Llen);
                  Source.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    Super_String_Data (New_Item);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Source.Current_Length := Max_Length;
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

   begin
      if Llen <= Max_Length - Rlen then
         Result.Data (1 .. Llen) := Super_String_Data (Left);

         if Rlen > 0 then
            Result.Data (Llen + 1 .. Llen + Rlen) := Right.Data (1 .. Rlen);
         end if;

         Result.Current_Length := Llen + Rlen;
      else
         case Drop is
            when Strings.Right =>
               if Llen >= Max_Length then
                  Result.Data (1 .. Max_Length) := Super_String_Data
                    (Left (Left'First .. Left'First + (Max_Length - 1)));

               else
                  Result.Data (1 .. Llen) := Super_String_Data (Left);
                  Result.Data (Llen + 1 .. Max_Length) :=
                    Right.Data (1 .. Max_Length - Llen);
               end if;

            when Strings.Left =>
               if Rlen >= Max_Length then
                  Result.Data (1 .. Max_Length) :=
                    Right.Data (Rlen - (Max_Length - 1) .. Rlen);

               else
                  Result.Data (1 .. Max_Length - Rlen) := Super_String_Data
                    (Left (Left'Last - (Max_Length - Rlen - 1) .. Left'Last));
                  Result.Data (Max_Length - Rlen + 1 .. Max_Length) :=
                    Right.Data (1 .. Rlen);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
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
         Result.Data (1 .. Llen) := Left.Data (1 .. Llen);
         Result.Data (Llen + 1) := Right;
         Result.Current_Length := Llen + 1;
         return Result;

      else
         case Drop is
            when Strings.Right =>
               return Left;

            when Strings.Left =>
               Result.Data (1 .. Max_Length - 1) :=
                 Left.Data (2 .. Max_Length);
               Result.Data (Max_Length) := Right;
               Result.Current_Length := Max_Length;
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
         Source.Data (Llen + 1) := New_Item;
         Source.Current_Length := Llen + 1;

      else
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
         Result.Data (1) := Left;
         Result.Data (2 .. Rlen + 1) := Right.Data (1 .. Rlen);
         Result.Current_Length := Rlen + 1;
         return Result;

      else
         case Drop is
            when Strings.Right =>
               Result.Data (1) := Left;
               Result.Data (2 .. Max_Length) :=
                 Right.Data (1 .. Max_Length - 1);
               Result.Current_Length := Max_Length;
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
      return Search.Count (Super_To_String (Source), Pattern, Mapping);
   end Super_Count;

   function Super_Count
     (Source  : Super_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return Search.Count (Super_To_String (Source), Pattern, Mapping);
   end Super_Count;

   function Super_Count
     (Source : Super_String;
      Set    : Maps.Character_Set) return Natural
   is
   begin
      return Search.Count (Super_To_String (Source), Set);
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

      elsif From - 1 > Slen then
         raise Ada.Strings.Index_Error;

      elsif Through >= Slen then
         Result.Data (1 .. From - 1) := Source.Data (1 .. From - 1);
         Result.Current_Length := From - 1;
         return Result;

      else
         Result.Data (1 .. From - 1) := Source.Data (1 .. From - 1);
         Result.Data (From .. Slen - Num_Delete) :=
           Source.Data (Through + 1 .. Slen);
         Result.Current_Length := Slen - Num_Delete;
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

      elsif From - 1 > Slen then
         raise Ada.Strings.Index_Error;

      elsif Through >= Slen then
         Source.Current_Length := From - 1;

      else
         Source.Current_Length := Slen - Num_Delete;
         Source.Data (From .. Source.Current_Length) :=
           Source.Data (Through + 1 .. Slen);
      end if;
   end Super_Delete;

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
        (Super_To_String (Source), Set, From, Test, First, Last);
   end Super_Find_Token;

   procedure Super_Find_Token
     (Source : Super_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin
      Search.Find_Token (Super_To_String (Source), Set, Test, First, Last);
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
         Result.Data (1 .. Count) := Source.Data (1 .. Count);
         Result.Current_Length := Count;

      elsif Count <= Max_Length then
         Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
         Result.Data (Slen + 1 .. Count) := [others => Pad];
         Result.Current_Length := Count;

      else
         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Slen) := Source.Data (1 .. Slen);

               if Slen < Max_Length then
                  Result.Data (Slen + 1 .. Max_Length) := [others => Pad];
               end if;

            when Strings.Left =>
               if Npad >= Max_Length then
                  Result.Data := [others => Pad];

               else
                  Result.Data (1 .. Max_Length - Npad) :=
                    Source.Data (Count - Max_Length + 1 .. Slen);
                  Result.Data (Max_Length - Npad + 1 .. Max_Length) :=
                    [others => Pad];
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
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
      Temp       : Super_String_Data (1 .. Max_Length);

   begin
      if Npad <= 0 then
         Source.Current_Length := Count;

      elsif Count <= Max_Length then
         Source.Data (Slen + 1 .. Count) := [others => Pad];
         Source.Current_Length := Count;

      else
         case Drop is
            when Strings.Right =>
               if Slen < Max_Length then
                  Source.Data (Slen + 1 .. Max_Length) := [others => Pad];
               end if;

            when Strings.Left =>
               if Npad > Max_Length then
                  Source.Data := [others => Pad];

               else
                  Temp := Source.Data;
                  Source.Data (1 .. Max_Length - Npad) :=
                    Temp (Count - Max_Length + 1 .. Slen);
                  Source.Data (Max_Length - Npad + 1 .. Max_Length) :=
                    [others => Pad];
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Source.Current_Length := Max_Length;
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
      return Search.Index (Super_To_String (Source), Pattern, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   is
   begin
      return Search.Index (Super_To_String (Source), Pattern, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source : Super_String;
      Set    : Maps.Character_Set;
      Test   : Strings.Membership := Strings.Inside;
      Going  : Strings.Direction  := Strings.Forward) return Natural
   is
   begin
      return Search.Index (Super_To_String (Source), Set, Test, Going);
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
        (Super_To_String (Source), Pattern, From, Going, Mapping);
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
        (Super_To_String (Source), Pattern, From, Going, Mapping);
   end Super_Index;

   function Super_Index
     (Source : Super_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   is
   begin
      return Result : Natural do
         Result :=
           Search.Index (Super_To_String (Source), Set, From, Test, Going);
         pragma Assert
           (if (for all J in 1 .. Super_Length (Source) =>
                  (if J = From or else (J > From) = (Going = Forward) then
                     (Test = Inside) /= Maps.Is_In (Source.Data (J), Set)))
            then Result = 0);
      end return;
   end Super_Index;

   ---------------------------
   -- Super_Index_Non_Blank --
   ---------------------------

   function Super_Index_Non_Blank
     (Source : Super_String;
      Going  : Strings.Direction := Strings.Forward) return Natural
   is
   begin
      return Search.Index_Non_Blank (Super_To_String (Source), Going);
   end Super_Index_Non_Blank;

   function Super_Index_Non_Blank
     (Source : Super_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
      return Search.Index_Non_Blank (Super_To_String (Source), From, Going);
   end Super_Index_Non_Blank;

   ------------------
   -- Super_Insert --
   ------------------

   function Super_Insert
     (Source   : Super_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Strings.Truncation := Strings.Error) return Super_String
   with SPARK_Mode => Off
   is
      Max_Length : constant Positive := Source.Max_Length;
      Result     : Super_String (Max_Length);
      Slen       : constant Natural := Source.Current_Length;
      Nlen       : constant Natural := New_Item'Length;
      Blen       : constant Natural := Before - 1;
      Alen       : constant Integer := Slen - Blen;
      Droplen    : constant Integer := Slen - Max_Length + Nlen;

      --  Blen, Alen are the lengths of the before and after pieces of the
      --  source string. The number of dropped characters is Natural'Max (0,
      --  Droplen).

   begin
      if Alen < 0 then
         raise Ada.Strings.Index_Error;

      elsif Droplen <= 0 then
         Result.Data (1 .. Blen) := Source.Data (1 .. Blen);
         Result.Data (Before .. Before - 1 + Nlen) :=
           Super_String_Data (New_Item);

         if Before <= Slen then
            Result.Data (Before + Nlen .. Slen + Nlen) :=
              Source.Data (Before .. Slen);
         end if;

         Result.Current_Length := Slen + Nlen;

      else
         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Blen) := Source.Data (1 .. Blen);

               if Droplen >= Alen then
                  Result.Data (Before .. Max_Length) := Super_String_Data
                    (New_Item (New_Item'First
                               .. New_Item'First - Before + Max_Length));
                  pragma Assert
                    (String (Result.Data (Before .. Max_Length)) =
                       New_Item (New_Item'First
                                 .. New_Item'First - Before + Max_Length));
               else
                  Result.Data (Before .. Before - 1 + Nlen) :=
                    Super_String_Data (New_Item);
                  Result.Data (Before + Nlen .. Max_Length) :=
                    Source.Data (Before .. Slen - Droplen);
               end if;

            when Strings.Left =>
               if Alen > 0 then
                  Result.Data (Max_Length - (Alen - 1) .. Max_Length) :=
                    Source.Data (Before .. Slen);
               end if;

               if Droplen > Blen then
                  if Alen < Max_Length then
                     Result.Data (1 .. Max_Length - Alen) := Super_String_Data
                       (New_Item (New_Item'Last - (Max_Length - Alen) + 1
                                  .. New_Item'Last));
                  end if;
               else
                  Result.Data (Blen - Droplen + 1 .. Max_Length - Alen) :=
                    Super_String_Data (New_Item);
                  Result.Data (1 .. Blen - Droplen) :=
                    Source.Data (Droplen + 1 .. Blen);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
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
      Slen       : constant Natural  := Source.Current_Length;
      Droplen    : Natural;

   begin
      if Position - 1 > Slen then
         raise Ada.Strings.Index_Error;

      elsif New_Item'Length = 0 then
         return Source;

      elsif Position - 1 <= Slen - New_Item'Length then
         Result.Data (1 .. Slen) := Source.Data (1 .. Slen);
         Result.Data (Position .. Position - 1 + New_Item'Length) :=
           Super_String_Data (New_Item);
         Result.Current_Length := Source.Current_Length;
         pragma Assert
           (String'(Super_Slice (Result, 1, Position - 1)) =
              Super_Slice (Source, 1, Position - 1));
         pragma Assert
           (Super_Slice (Result,
            Position, Position - 1 + New_Item'Length) =
              New_Item);

         return Result;

      elsif Position - 1 <= Max_Length - New_Item'Length then
         Result.Data (1 .. Position - 1) := Source.Data (1 .. Position - 1);
         Result.Data (Position .. Position - 1 + New_Item'Length) :=
           Super_String_Data (New_Item);
         Result.Current_Length := Position - 1 + New_Item'Length;
         pragma Assert
           (String'(Super_Slice (Result, 1, Position - 1)) =
              Super_Slice (Source, 1, Position - 1));
         pragma Assert
           (Super_Slice (Result,
            Position, Position - 1 + New_Item'Length) =
              New_Item);

         return Result;

      else
         Droplen := Position - 1 - Max_Length + New_Item'Length;

         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Position - 1) :=
                 Source.Data (1 .. Position - 1);

               Result.Data (Position .. Max_Length) := Super_String_Data
                 (New_Item (New_Item'First .. New_Item'Last - Droplen));

            when Strings.Left =>
               if New_Item'Length >= Max_Length then
                  Result.Data (1 .. Max_Length) := Super_String_Data
                    (New_Item (New_Item'Last - Max_Length + 1 ..
                                New_Item'Last));

               else
                  Result.Data (1 .. Max_Length - New_Item'Length) :=
                    Source.Data (Droplen + 1 .. Position - 1);
                  Result.Data
                    (Max_Length - New_Item'Length + 1 .. Max_Length) :=
                    Super_String_Data (New_Item);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
         pragma Assert (Super_Length (Result) = Source.Max_Length);
         return Result;
      end if;
   end Super_Overwrite;

   procedure Super_Overwrite
     (Source    : in out Super_String;
      Position  : Positive;
      New_Item  : String;
      Drop      : Strings.Truncation := Strings.Error)
   with SPARK_Mode => Off
   is
      Max_Length : constant Positive := Source.Max_Length;
      Slen       : constant Natural  := Source.Current_Length;
      Droplen    : Natural;

   begin
      if Position - 1 > Slen then
         raise Ada.Strings.Index_Error;

      elsif Position - 1 <= Slen - New_Item'Length then
         Source.Data (Position .. Position - 1 + New_Item'Length) :=
           Super_String_Data (New_Item);

      elsif Position - 1 <= Max_Length - New_Item'Length then
         Source.Data (Position .. Position - 1 + New_Item'Length) :=
           Super_String_Data (New_Item);
         Source.Current_Length := Position - 1 + New_Item'Length;

      else
         Droplen := Position - 1 - Max_Length + New_Item'Length;

         case Drop is
            when Strings.Right =>
               Source.Data (Position .. Max_Length) := Super_String_Data
                 (New_Item (New_Item'First .. New_Item'Last - Droplen));

            when Strings.Left =>
               if New_Item'Length >= Max_Length then
                  Source.Data (1 .. Max_Length) := Super_String_Data
                    (New_Item
                      (New_Item'Last - Max_Length + 1 .. New_Item'Last));

               else
                  Source.Data (1 .. Max_Length - New_Item'Length) :=
                    Source.Data (Droplen + 1 .. Position - 1);
                  Source.Data
                    (Max_Length - New_Item'Length + 1 .. Max_Length) :=
                    Super_String_Data (New_Item);
               end if;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Source.Current_Length := Max_Length;
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
   with SPARK_Mode => Off
   is
      Max_Length : constant Positive := Source.Max_Length;
      Slen       : constant Natural  := Source.Current_Length;

   begin
      if Low - 1 > Slen then
         raise Strings.Index_Error;

      elsif High < Low then
         return Super_Insert (Source, Low, By, Drop);

      else
         declare
            Blen    : constant Natural := Low - 1;
            Alen    : constant Natural := Natural'Max (0, Slen - High);
            Droplen : constant Integer := Blen + Alen - Max_Length + By'Length;
            Result  : Super_String (Max_Length);

            --  Blen and Alen are the lengths of the pieces of the original
            --  string that end up in the result string before and after the
            --  replaced slice. The number of dropped characters is Natural'Max
            --  (0, Droplen).

         begin
            if Droplen <= 0 then
               Result.Data (1 .. Blen) := Source.Data (1 .. Blen);
               Result.Data (Low .. Blen + By'Length) :=
                 Super_String_Data (By);

               if Alen > 0 then
                  Result.Data (Low + By'Length .. Blen + By'Length + Alen) :=
                    Source.Data (High + 1 .. Slen);
               end if;

               Result.Current_Length := Blen + By'Length + Alen;

            else
               case Drop is
                  when Strings.Right =>
                     Result.Data (1 .. Blen) := Source.Data (1 .. Blen);

                     if Droplen >= Alen then
                        Result.Data (Low .. Max_Length) := Super_String_Data
                          (By (By'First .. By'First - Low + Max_Length));
                     else
                        Result.Data (Low .. Low - 1 + By'Length) :=
                          Super_String_Data (By);
                        Result.Data (Low + By'Length .. Max_Length) :=
                          Source.Data (High + 1 .. Slen - Droplen);
                     end if;

                  when Strings.Left =>
                     if Alen > 0 then
                        Result.Data (Max_Length - (Alen - 1) .. Max_Length) :=
                          Source.Data (High + 1 .. Slen);
                     end if;

                     if Droplen >= Blen then
                        Result.Data (1 .. Max_Length - Alen) :=
                          Super_String_Data (By
                            (By'Last - (Max_Length - Alen) + 1 .. By'Last));
                     else
                        Result.Data
                          (Blen - Droplen + 1 .. Max_Length - Alen) :=
                            Super_String_Data (By);
                        Result.Data (1 .. Blen - Droplen) :=
                          Source.Data (Droplen + 1 .. Blen);
                     end if;

                  when Strings.Error =>
                     raise Ada.Strings.Length_Error;
               end case;

               Result.Current_Length := Max_Length;
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
         Result.Data (1 .. Count) := [others => Item];
         Result.Current_Length := Count;

      elsif Drop = Strings.Error then
         raise Ada.Strings.Length_Error;

      else
         Result.Data (1 .. Max_Length) := [others => Item];
         Result.Current_Length := Max_Length;
      end if;

      return Result;
   end Super_Replicate;

   function Super_Replicate
     (Count      : Natural;
      Item       : String;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String
   is
      Result : Super_String (Max_Length);
      Indx   : Natural;
      Ilen   : constant Natural := Item'Length;

      --  Parts of the proof involving manipulations with the modulo operator
      --  are complicated for the prover and can't be done automatically in
      --  the global subprogram. That's why we isolate them in these two ghost
      --  lemmas.

      procedure Lemma_Mod (K : Natural; Q : Natural) with
        Ghost,
        Pre  => Ilen /= 0
          and then Q mod Ilen = 0
          and then K - Q in 0 .. Ilen - 1,
        Post => K mod Ilen = K - Q;
      --  Lemma_Mod is applied to an index considered in Lemma_Split to prove
      --  that it has the right value modulo Item'Length.

      procedure Lemma_Mod_Zero (X : Natural) with
        Ghost,
        Pre  => Ilen /= 0
          and then X mod Ilen = 0
          and then X <= Natural'Last - Ilen,
        Post => (X + Ilen) mod Ilen = 0;
      --  Lemma_Mod_Zero is applied to prove that the length of the range
      --  of indexes considered in the loop, when dropping on the Left, is
      --  a multiple of Item'Length.

      procedure Lemma_Split (Going : Direction) with
        Ghost,
        Pre  =>
          Ilen /= 0
            and then Indx in 0 .. Max_Length - Ilen
            and then
              (if Going = Forward
               then Indx mod Ilen = 0
               else (Max_Length - Indx - Ilen) mod Ilen = 0)
            and then Result.Data (Indx + 1 .. Indx + Ilen)'Initialized
            and then String (Result.Data (Indx + 1 .. Indx + Ilen)) = Item,
        Post =>
          (if Going = Forward then
             (for all J in Indx + 1 .. Indx + Ilen =>
                Result.Data (J) = Item (Item'First + (J - 1) mod Ilen))
           else
             (for all J in Indx + 1 .. Indx + Ilen =>
                Result.Data (J) =
                  Item (Item'Last - (Max_Length - J) mod Ilen)));
      --  Lemma_Split is used after Result.Data (Indx + 1 .. Indx + Ilen) is
      --  updated to Item and concludes that the characters match for each
      --  index when taken modulo Item'Length, as the considered slice starts
      --  at index 1 (or ends at index Max_Length, if Going = Backward) modulo
      --  Item'Length.

      ---------------
      -- Lemma_Mod --
      ---------------

      procedure Lemma_Mod (K : Natural; Q : Natural) is null;

      --------------------
      -- Lemma_Mod_Zero --
      --------------------

      procedure Lemma_Mod_Zero (X : Natural) is null;

      -----------------
      -- Lemma_Split --
      -----------------

      procedure Lemma_Split (Going : Direction) is
      begin
         if Going = Forward then
            for K in Indx + 1 .. Indx + Ilen loop
               Lemma_Mod (K - 1, Indx);
               pragma Loop_Invariant
                 (for all J in Indx + 1 .. K =>
                    Result.Data (J) = Item (Item'First + (J - 1) mod Ilen));
            end loop;
         else
            for K in Indx + 1 .. Indx + Ilen loop
               Lemma_Mod (Max_Length - K, Max_Length - Indx - Ilen);
               pragma Loop_Invariant
                 (for all J in Indx + 1 .. K =>
                    Result.Data (J) =
                      Item (Item'Last - (Max_Length - J) mod Ilen));
            end loop;
         end if;
      end Lemma_Split;

   begin
      if Count = 0 or else Ilen <= Max_Length / Count then
         if Count * Ilen > 0 then
            Indx := 0;

            for J in 1 .. Count loop
               Result.Data (Indx + 1 .. Indx + Ilen) :=
                 Super_String_Data (Item);
               pragma Assert
                 (for all K in 1 .. Ilen =>
                    Result.Data (Indx + K) = Item (Item'First - 1 + K));
               pragma Assert
                 (String (Result.Data (Indx + 1 .. Indx + Ilen)) = Item);
               Lemma_Split (Forward);
               Indx := Indx + Ilen;
               pragma Loop_Invariant (Indx = J * Ilen);
               pragma Loop_Invariant (Result.Data (1 .. Indx)'Initialized);
               pragma Loop_Invariant
                 (for all K in 1 .. Indx =>
                    Result.Data (K) =
                      Item (Item'First + (K - 1) mod Ilen));
            end loop;
         end if;

         Result.Current_Length := Count * Ilen;

      else
         case Drop is
            when Strings.Right =>
               Indx := 0;

               while Indx < Max_Length - Ilen loop
                  Result.Data (Indx + 1 .. Indx + Ilen) :=
                    Super_String_Data (Item);
                  pragma Assert
                    (for all K in 1 .. Ilen =>
                       Result.Data (Indx + K) = Item (Item'First - 1 + K));
                  pragma Assert
                    (String (Result.Data (Indx + 1 .. Indx + Ilen)) = Item);
                  Lemma_Split (Forward);
                  Indx := Indx + Ilen;
                  pragma Loop_Invariant (Indx mod Ilen = 0);
                  pragma Loop_Invariant (Indx in 0 .. Max_Length - 1);
                  pragma Loop_Invariant (Result.Data (1 .. Indx)'Initialized);
                  pragma Loop_Invariant
                    (for all K in 1 .. Indx =>
                       Result.Data (K) =
                         Item (Item'First + (K - 1) mod Ilen));
               end loop;

               Result.Data (Indx + 1 .. Max_Length) := Super_String_Data
                 (Item (Item'First .. Item'First + (Max_Length - Indx - 1)));
               pragma Assert
                 (for all J in Indx + 1 .. Max_Length =>
                    Result.Data (J) = Item (Item'First - 1 - Indx + J));

               for J in Indx + 1 .. Max_Length loop
                  Lemma_Mod (J - 1, Indx);
                  pragma Loop_Invariant
                    (for all K in 1 .. J =>
                       Result.Data (K) =
                         Item (Item'First + (K - 1) mod Ilen));
               end loop;

            when Strings.Left =>
               Indx := Max_Length;

               while Indx > Ilen loop
                  Indx := Indx - Ilen;
                  Result.Data (Indx + 1 .. Indx + Ilen) :=
                    Super_String_Data (Item);
                  pragma Assert
                    (for all K in 1 .. Ilen =>
                       Result.Data (Indx + K) = Item (Item'First - 1 + K));
                  pragma Assert
                    (String (Result.Data (Indx + 1 .. Indx + Ilen)) = Item);
                  Lemma_Split (Backward);
                  Lemma_Mod_Zero (Max_Length - Indx - Ilen);
                  pragma Loop_Invariant
                    ((Max_Length - Indx) mod Ilen = 0);
                  pragma Loop_Invariant (Indx in 1 .. Max_Length);
                  pragma Loop_Invariant
                    (Result.Data (Indx + 1 .. Max_Length)'Initialized);
                  pragma Loop_Invariant
                    (for all K in Indx + 1 .. Max_Length =>
                       Result.Data (K) =
                         Item (Item'Last - (Max_Length - K) mod Ilen));
               end loop;

               Result.Data (1 .. Indx) :=
                 Super_String_Data (Item (Item'Last - Indx + 1 .. Item'Last));
               pragma Assert
                 (for all J in 1 .. Indx =>
                    Result.Data (J) = Item (Item'Last - Indx + J));

               for J in reverse 1 .. Indx loop
                  Lemma_Mod (Max_Length - J, Max_Length - Indx);
                  pragma Loop_Invariant
                    (for all K in J .. Max_Length =>
                       Result.Data (K) =
                         Item (Item'Last - (Max_Length - K) mod Ilen));
               end loop;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
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
        Super_Replicate (Count, Super_To_String (Item), Drop, Item.Max_Length);
   end Super_Replicate;

   -----------------
   -- Super_Slice --
   -----------------

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return Super_String
   is
   begin
      return Result : Super_String (Source.Max_Length) do
         if Low - 1 > Source.Current_Length
           or else High > Source.Current_Length
         then
            raise Index_Error;
         end if;

         Result.Current_Length := (if Low > High then 0 else High - Low + 1);
         Result.Data (1 .. Result.Current_Length) :=
           Source.Data (Low .. High);
      end return;
   end Super_Slice;

   procedure Super_Slice
     (Source : Super_String;
      Target : out Super_String;
      Low    : Positive;
      High   : Natural)
   is
   begin
      if Low - 1 > Source.Current_Length
        or else High > Source.Current_Length
      then
         raise Index_Error;
      end if;

      Target.Current_Length := (if Low > High then 0 else High - Low + 1);
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
         if Count > 0 then
            Result.Data (1 .. Count) :=
              Source.Data (Slen - (Count - 1) .. Slen);
            Result.Current_Length := Count;
         end if;

      elsif Count <= Max_Length then
         Result.Data (1 .. Npad) := [others => Pad];

         if Slen > 0 then
            Result.Data (Npad + 1 .. Count) := Source.Data (1 .. Slen);
         end if;

         Result.Current_Length := Count;

      else
         case Drop is
            when Strings.Right =>
               if Npad >= Max_Length then
                  Result.Data := [others => Pad];

               else
                  Result.Data (1 .. Npad) := [others => Pad];
                  Result.Data (Npad + 1 .. Max_Length) :=
                    Source.Data (1 .. Max_Length - Npad);
               end if;

            when Strings.Left =>
               Result.Data (1 .. Max_Length - Slen) := [others => Pad];
               Result.Data (Max_Length - Slen + 1 .. Max_Length) :=
                 Source.Data (1 .. Slen);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Result.Current_Length := Max_Length;
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

      Temp : constant Super_String_Data (1 .. Max_Length) := Source.Data;

   begin
      if Npad <= 0 then
         Source.Current_Length := Count;

         if Count > 0 then
            Source.Data (1 .. Count) :=
              Temp (Slen - (Count - 1) .. Slen);
         end if;

      elsif Count <= Max_Length then
         Source.Data (1 .. Npad) := [others => Pad];

         if Slen > 0 then
            Source.Data (Npad + 1 .. Count) := Temp (1 .. Slen);
         end if;

         Source.Current_Length := Count;

      else
         case Drop is
            when Strings.Right =>
               if Npad >= Max_Length then
                  Source.Data := [others => Pad];

               else
                  Source.Data (1 .. Npad) := [others => Pad];
                  Source.Data (Npad + 1 .. Max_Length) :=
                    Temp (1 .. Max_Length - Npad);
               end if;

            when Strings.Left =>
               Source.Data (1 .. Max_Length - Slen) := [others => Pad];
               Source.Data (Max_Length - Slen + 1 .. Max_Length) :=
                 Temp (1 .. Slen);

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;

         Source.Current_Length := Max_Length;
      end if;
   end Super_Tail;

   ---------------------
   -- Super_Translate --
   ---------------------

   function Super_Translate
     (Source  : Super_String;
      Mapping : Maps.Character_Mapping) return Super_String
   is
      Result : Super_String (Source.Max_Length);

   begin
      for J in 1 .. Source.Current_Length loop
         Result.Data (J) := Value (Mapping, Source.Data (J));
         pragma Loop_Invariant (Result.Data (1 .. J)'Initialized);
         pragma Loop_Invariant
           (for all K in 1 .. J =>
              Result.Data (K) = Value (Mapping, Source.Data (K)));
      end loop;

      Result.Current_Length := Source.Current_Length;
      return Result;
   end Super_Translate;

   procedure Super_Translate
     (Source  : in out Super_String;
      Mapping : Maps.Character_Mapping)
   is
   begin
      for J in 1 .. Source.Current_Length loop
         Source.Data (J) := Value (Mapping, Source.Data (J));
         pragma Loop_Invariant
           (for all K in 1 .. J =>
              Source.Data (K) = Value (Mapping, Source'Loop_Entry.Data (K)));
      end loop;
   end Super_Translate;

   function Super_Translate
     (Source  : Super_String;
      Mapping : Maps.Character_Mapping_Function) return Super_String
   is
      Result : Super_String (Source.Max_Length);

   begin
      for J in 1 .. Source.Current_Length loop
         Result.Data (J) := Mapping.all (Source.Data (J));
         pragma Loop_Invariant (Result.Data (1 .. J)'Initialized);
         pragma Loop_Invariant
           (for all K in 1 .. J =>
              Result.Data (K) = Mapping (Source.Data (K)));
      end loop;

      Result.Current_Length := Source.Current_Length;
      return Result;
   end Super_Translate;

   procedure Super_Translate
     (Source  : in out Super_String;
      Mapping : Maps.Character_Mapping_Function)
   is
   begin
      for J in 1 .. Source.Current_Length loop
         Source.Data (J) := Mapping.all (Source.Data (J));
         pragma Loop_Invariant
           (for all K in 1 .. J =>
              Source.Data (K) = Mapping (Source'Loop_Entry.Data (K)));
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
      Last   : constant Natural := Source.Current_Length;

   begin
      case Side is
         when Strings.Left =>
            declare
               Low : constant Natural :=
                 Super_Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return Result;
               end if;

               Result.Data (1 .. Last - Low + 1) := Source.Data (Low .. Last);
               Result.Current_Length := Last - Low + 1;
            end;

         when Strings.Right =>
            declare
               High : constant Natural :=
                 Super_Index_Non_Blank (Source, Backward);
            begin
               --  All blanks case

               if High = 0 then
                  return Result;
               end if;

               Result.Data (1 .. High) := Source.Data (1 .. High);
               Result.Current_Length := High;
            end;

         when Strings.Both =>
            declare
               Low : constant Natural :=
                 Super_Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return Result;
               end if;

               declare
                  High : constant Natural :=
                    Super_Index_Non_Blank (Source, Backward);
               begin
                  Result.Data (1 .. High - Low + 1) :=
                    Source.Data (Low .. High);
                  Result.Current_Length := High - Low + 1;
               end;
            end;
      end case;

      return Result;
   end Super_Trim;

   procedure Super_Trim
     (Source : in out Super_String;
      Side   : Trim_End)
   is
      Last : constant Natural := Source.Current_Length;
   begin
      case Side is
         when Strings.Left =>
            declare
               Low : constant Natural :=
                 Super_Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  Source.Current_Length := 0;
               else
                  Source.Data (1 .. Last - Low + 1) :=
                    Source.Data (Low .. Last);
                  Source.Current_Length := Last - Low + 1;
               end if;
            end;

         when Strings.Right =>
            declare
               High : constant Natural :=
                 Super_Index_Non_Blank (Source, Backward);
            begin
               Source.Current_Length := High;
            end;

         when Strings.Both =>
            declare
               Low : constant Natural :=
                 Super_Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  Source.Current_Length := 0;
               else
                  declare
                     High : constant Natural :=
                       Super_Index_Non_Blank (Source, Backward);
                  begin
                     Source.Data (1 .. High - Low + 1) :=
                       Source.Data (Low .. High);
                     Source.Current_Length := High - Low + 1;
                  end;
               end if;
            end;
      end case;
   end Super_Trim;

   function Super_Trim
     (Source : Super_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Super_String
   is
      Result : Super_String (Source.Max_Length);
      Low    : Natural;
      High   : Natural;

   begin
      Low := Super_Index (Source, Left, Outside, Forward);

      --  Case where source comprises only characters in Left

      if Low = 0 then
         return Result;
      end if;

      High := Super_Index (Source, Right, Outside, Backward);

      --  Case where source comprises only characters in Right

      if High = 0 then
         return Result;
      end if;

      if High >= Low then
         Result.Data (1 .. High - Low + 1) := Source.Data (Low .. High);
         Result.Current_Length := High - Low + 1;
      end if;

      return Result;
   end Super_Trim;

   procedure Super_Trim
     (Source : in out Super_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set)
   is
      Last : constant Natural := Source.Current_Length;
      Temp : Super_String_Data (1 .. Last);
      Low  : Natural;
      High : Natural;

   begin
      Temp := Source.Data (1 .. Last);
      Low := Super_Index (Source, Left, Outside, Forward);

      --  Case where source comprises only characters in Left

      if Low = 0 then
         Source.Current_Length := 0;

      else
         High := Super_Index (Source, Right, Outside, Backward);

         --  Case where source comprises only characters in Right

         if High = 0 then
            Source.Current_Length := 0;

         elsif Low = 1 then
            Source.Current_Length := High;

         elsif High < Low then
            Source.Current_Length := 0;

         else
            Source.Data (1 .. High - Low + 1) := Temp (Low .. High);
            Source.Current_Length := High - Low + 1;
         end if;
      end if;
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
         for J in 1 .. Left loop
            Result.Data (J) := Right;
            pragma Loop_Invariant (Result.Data (1 .. J)'Initialized);
            pragma Loop_Invariant
              (for all K in 1 .. J => Result.Data (K) = Right);
         end loop;

         Result.Current_Length := Left;
      end if;

      return Result;
   end Times;

   function Times
     (Left       : Natural;
      Right      : String;
      Max_Length : Positive) return Super_String
   is
      Result : Super_String (Max_Length);
      Pos    : Natural          := 0;
      Rlen   : constant Natural := Right'Length;
      Nlen   : constant Natural := Left * Rlen;

      --  Parts of the proof involving manipulations with the modulo operator
      --  are complicated for the prover and can't be done automatically in
      --  the global subprogram. That's why we isolate them in these two ghost
      --  lemmas.

      procedure Lemma_Mod (K : Integer) with
        Ghost,
        Pre =>
          Rlen /= 0
          and then Pos mod Rlen = 0
          and then Pos in 0 .. Max_Length - Rlen
          and then K in Pos .. Pos + Rlen - 1,
        Post => K mod Rlen = K - Pos;
      --  Lemma_Mod is applied to an index considered in Lemma_Split to prove
      --  that it has the right value modulo Right'Length.

      procedure Lemma_Split with
        Ghost,
        Pre  =>
          Rlen /= 0
            and then Pos mod Rlen = 0
            and then Pos in 0 .. Max_Length - Rlen
            and then Result.Data (1 .. Pos + Rlen)'Initialized
            and then String (Result.Data (Pos + 1 .. Pos + Rlen)) = Right,
        Post =>
          (for all K in Pos + 1 .. Pos + Rlen =>
            Result.Data (K) = Right (Right'First + (K - 1) mod Rlen));
      --  Lemma_Split is used after Result.Data (Pos + 1 .. Pos + Rlen) is
      --  updated to Right and concludes that the characters match for each
      --  index when taken modulo Right'Length, as the considered slice starts
      --  at index 1 modulo Right'Length.

      ---------------
      -- Lemma_Mod --
      ---------------

      procedure Lemma_Mod (K : Integer) is null;

      -----------------
      -- Lemma_Split --
      -----------------

      procedure Lemma_Split is
      begin
         for K in Pos + 1 .. Pos + Rlen loop
            Lemma_Mod (K - 1);
            pragma Loop_Invariant
              (for all J in Pos + 1 .. K =>
                 Result.Data (J) = Right (Right'First + (J - 1) mod Rlen));
         end loop;
      end Lemma_Split;

   begin
      if Nlen > Max_Length then
         raise Ada.Strings.Length_Error;

      else
         if Nlen > 0 then
            for J in 1 .. Left loop
               Result.Data (Pos + 1 .. Pos + Rlen) :=
                 Super_String_Data (Right);
               pragma Assert
                 (for all K in 1 .. Rlen => Result.Data (Pos + K) =
                    Right (Right'First - 1 + K));
               pragma Assert
                 (String (Result.Data (Pos + 1 .. Pos + Rlen)) = Right);
               Lemma_Split;
               Pos := Pos + Rlen;
               pragma Loop_Invariant (Pos = J * Rlen);
               pragma Loop_Invariant (Result.Data (1 .. Pos)'Initialized);
               pragma Loop_Invariant
                 (for all K in 1 .. Pos =>
                    Result.Data (K) =
                      Right (Right'First + (K - 1) mod Rlen));
            end loop;
         end if;

         Result.Current_Length := Nlen;
      end if;

      return Result;
   end Times;

   function Times
     (Left  : Natural;
      Right : Super_String) return Super_String
   is
      Result : Super_String (Right.Max_Length);
      Pos    : Natural          := 0;
      Rlen   : constant Natural := Right.Current_Length;
      Nlen   : constant Natural := Left * Rlen;

   begin
      if Nlen > Right.Max_Length then
         raise Ada.Strings.Length_Error;

      else
         if Nlen > 0 then
            for J in 1 .. Left loop
               Result.Data (Pos + 1 .. Pos + Rlen) :=
                 Right.Data (1 .. Rlen);
               Pos := Pos + Rlen;
               pragma Loop_Invariant (Pos = J * Rlen);
               pragma Loop_Invariant (Result.Data (1 .. Pos)'Initialized);
               pragma Loop_Invariant
                 (for all K in 1 .. Pos =>
                    Result.Data (K) =
                      Right.Data (1 + (K - 1) mod Rlen));
            end loop;
         end if;

         Result.Current_Length := Nlen;
      end if;

      return Result;
   end Times;

   ---------------------
   -- To_Super_String --
   ---------------------

   function To_Super_String
     (Source     : String;
      Max_Length : Positive;
      Drop       : Truncation := Error) return Super_String
   is
      Result : Super_String (Max_Length);
      Slen   : constant Natural := Source'Length;

   begin
      if Slen <= Max_Length then
         Result.Data (1 .. Slen) := Super_String_Data (Source);
         Result.Current_Length := Slen;

      else
         case Drop is
            when Strings.Right =>
               Result.Data (1 .. Max_Length) := Super_String_Data
                 (Source (Source'First .. Source'First - 1 + Max_Length));
               Result.Current_Length := Max_Length;

            when Strings.Left =>
               Result.Data (1 .. Max_Length) := Super_String_Data
                 (Source (Source'Last - (Max_Length - 1) .. Source'Last));
               Result.Current_Length := Max_Length;

            when Strings.Error =>
               raise Ada.Strings.Length_Error;
         end case;
      end if;

      return Result;
   end To_Super_String;

end Ada.Strings.Superbounded;
