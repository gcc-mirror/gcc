------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S T R I N G S . F I X E D                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  Note: This code is derived from the ADAR.CSH public domain Ada 83 versions
--  of the Appendix C string handling packages. One change is to avoid the use
--  of Is_In, so that we are not dependent on inlining. Note that the search
--  function implementations are to be found in the auxiliary package
--  Ada.Strings.Search. Also the Move procedure is directly incorporated (ADAR
--  used a subunit for this procedure). The number of errors having to do with
--  bounds of function return results were also fixed, and use of & removed for
--  efficiency reasons.

--  Ghost code, loop invariants and assertions in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with Ada.Strings.Maps; use Ada.Strings.Maps;

package body Ada.Strings.Fixed with SPARK_Mode is

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   renames Ada.Strings.Search.Index;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   renames Ada.Strings.Search.Index;

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   renames Ada.Strings.Search.Index_Non_Blank;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   renames Ada.Strings.Search.Index_Non_Blank;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   renames Ada.Strings.Search.Count;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   renames Ada.Strings.Search.Count;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
   renames Ada.Strings.Search.Count;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   renames Ada.Strings.Search.Find_Token;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   renames Ada.Strings.Search.Find_Token;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character) return String
   is
   begin
      return Result : String (1 .. Left) with Relaxed_Initialization do
         for J in Result'Range loop
            Result (J) := Right;
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                 Result (K)'Initialized and then Result (K) = Right);
         end loop;
      end return;
   end "*";

   function "*"
     (Left  : Natural;
      Right : String) return String
   is
      Ptr : Integer := 0;

      --  Parts of the proof involving manipulations with the modulo operator
      --  are complicated for the prover and can't be done automatically in
      --  the global subprogram. That's why we isolate them in these two ghost
      --  lemmas.

      procedure Lemma_Mod (K : Integer) with
        Ghost,
        Pre =>
          Right'Length /= 0
          and then Ptr mod Right'Length = 0
          and then Ptr in 0 .. Natural'Last - Right'Length
          and then K in Ptr .. Ptr + Right'Length - 1,
        Post => K mod Right'Length = K - Ptr;
      --  Lemma_Mod is applied to an index considered in Lemma_Split to prove
      --  that it has the right value modulo Right'Length.

      procedure Lemma_Split (Result : String) with
        Ghost,
        Relaxed_Initialization => Result,
        Pre                    =>
          Right'Length /= 0
            and then Result'First = 1
            and then Result'Last >= 0
            and then Ptr mod Right'Length = 0
            and then Ptr in 0 .. Result'Last - Right'Length
            and then Result (Result'First .. Ptr + Right'Length)'Initialized
            and then Result (Ptr + 1 .. Ptr + Right'Length) = Right,
        Post                   =>
          (for all K in Ptr + 1 .. Ptr + Right'Length =>
            Result (K) = Right (Right'First + (K - 1) mod Right'Length));
      --  Lemma_Split is used after Result (Ptr + 1 .. Ptr + Right'Length) is
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

      procedure Lemma_Split (Result : String)
      is
      begin
         for K in Ptr + 1 .. Ptr + Right'Length loop
            Lemma_Mod (K - 1);
            pragma Loop_Invariant
              (for all J in Ptr + 1 .. K =>
                 Result (J) = Right (Right'First + (J - 1) mod Right'Length));
         end loop;
      end Lemma_Split;

   --  Start of processing for "*"

   begin
      if Right'Length = 0 then
         return "";
      end if;

      return Result : String (1 .. Left * Right'Length)
        with Relaxed_Initialization
      do
         for J in 1 .. Left loop
            Result (Ptr + 1 .. Ptr + Right'Length) := Right;
            Lemma_Split (Result);
            Ptr := Ptr + Right'Length;
            pragma Loop_Invariant (Ptr = J * Right'Length);
            pragma Loop_Invariant (Result (1 .. Ptr)'Initialized);
            pragma Loop_Invariant
              (for all K in 1 .. Ptr =>
                 Result (K) = Right (Right'First + (K - 1) mod Right'Length));
         end loop;
      end return;
   end "*";

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String
   is
   begin
      if From > Through then
         declare
            subtype Result_Type is String (1 .. Source'Length);

         begin
            return Result_Type (Source);
         end;

      elsif From not in Source'Range
        or else Through > Source'Last
      then
         pragma Annotate
           (CodePeer, False_Positive,
            "test always false", "self fullfilling prophecy");

         --  In most cases this raises an exception, but the case of deleting
         --  a null string at the end of the current one is a special-case, and
         --  reflects the equivalence with Replace_String (RM A.4.3 (86/3)).

         if From = Source'Last + 1 and then From = Through then
            return Source;
         else
            raise Index_Error;
         end if;

      else
         declare
            Front : constant Integer := From - Source'First;

         begin
            return Result : String (1 .. Source'Length - (Through - From + 1))
              with Relaxed_Initialization
            do
               Result (1 .. Front) :=
                 Source (Source'First .. From - 1);

               if Through < Source'Last then
                  Result (Front + 1 .. Result'Last) :=
                    Source (Through + 1 .. Source'Last);
               end if;
            end return;
         end;
      end if;
   end Delete;

   procedure Delete
     (Source  : in out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Delete (Source, From, Through),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Delete;

   ----------
   -- Head --
   ----------

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      subtype Result_Type is String (1 .. Count);

   begin
      if Count <= Source'Length then
         return
           Result_Type (Source (Source'First .. Source'First + (Count - 1)));

      else
         return Result : Result_Type with Relaxed_Initialization do
            Result (1 .. Source'Length) := Source;

            for J in Source'Length + 1 .. Count loop
               Result (J) := Pad;
               pragma Loop_Invariant
                 (for all K in Source'Length + 1 .. J =>
                    Result (K)'Initialized and then Result (K) = Pad);
            end loop;
         end return;
      end if;
   end Head;

   procedure Head
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Head (Source, Count, Pad),
            Target  => Source,
            Drop    => Error,
            Justify => Justify,
            Pad     => Pad);
   end Head;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String
   is
      Front  : constant Integer := Before - Source'First;

   begin
      if Before - 1 not in Source'First - 1 .. Source'Last then
         raise Index_Error;
      end if;

      return Result : String (1 .. Source'Length + New_Item'Length)
        with Relaxed_Initialization
      do
         Result (1 .. Front) :=
           Source (Source'First .. Before - 1);
         Result (Front + 1 .. Front + New_Item'Length) :=
           New_Item;
         pragma Assert
           (Result
              (Before - Source'First + 1
               .. Before - Source'First + New_Item'Length)
            = New_Item);

         if Before <= Source'Last then
            Result (Front + New_Item'Length + 1 .. Result'Last) :=
              Source (Before .. Source'Last);
         end if;

         pragma Assert
           (Result (1 .. Before - Source'First)
            = Source (Source'First .. Before - 1));
      end return;
   end Insert;

   procedure Insert
     (Source   : in out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   is
   begin
      Move (Source => Insert (Source, Before, New_Item),
            Target => Source,
            Drop   => Drop);
   end Insert;

   ----------
   -- Move --
   ----------

   procedure Move
     (Source  : String;
      Target  : out String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space)
   with SPARK_Mode => Off
   is
      Sfirst  : constant Integer := Source'First;
      Slast   : constant Integer := Source'Last;
      Slength : constant Integer := Source'Length;

      Tfirst  : constant Integer := Target'First;
      Tlast   : constant Integer := Target'Last;
      Tlength : constant Integer := Target'Length;

      function Is_Padding (Item : String) return Boolean;
      --  Check if Item is all Pad characters, return True if so, False if not

      function Is_Padding (Item : String) return Boolean is
      begin
         for J in Item'Range loop
            if Item (J) /= Pad then
               return False;
            end if;
         end loop;

         return True;
      end Is_Padding;

   --  Start of processing for Move

   begin
      if Slength = Tlength then
         Target := Source;

      elsif Slength > Tlength then
         case Drop is
            when Left =>
               Target := Source (Slast - Tlength + 1 .. Slast);

            when Right =>
               Target := Source (Sfirst .. Sfirst + Tlength - 1);

            when Error =>
               case Justify is
                  when Left =>
                     if Is_Padding (Source (Sfirst + Tlength .. Slast)) then
                        Target :=
                          Source (Sfirst .. Sfirst + Target'Length - 1);
                     else
                        raise Length_Error;
                     end if;

                  when Right =>
                     if Is_Padding (Source (Sfirst .. Slast - Tlength)) then
                        Target := Source (Slast - Tlength + 1 .. Slast);
                     else
                        raise Length_Error;
                     end if;

                  when Center =>
                     raise Length_Error;
               end case;
         end case;

      --  Source'Length < Target'Length

      else
         case Justify is
            when Left =>
               Target (Tfirst .. Tfirst + Slength - 1) := Source;

               for I in Tfirst + Slength .. Tlast loop
                  Target (I) := Pad;
               end loop;

            when Right =>
               for I in Tfirst .. Tlast - Slength loop
                  Target (I) := Pad;
               end loop;

               Target (Tlast - Slength + 1 .. Tlast) := Source;

            when Center =>
               declare
                  Front_Pad   : constant Integer := (Tlength - Slength) / 2;
                  Tfirst_Fpad : constant Integer := Tfirst + Front_Pad;

               begin
                  for I in Tfirst .. Tfirst_Fpad - 1 loop
                     Target (I) := Pad;
                  end loop;

                  Target (Tfirst_Fpad .. Tfirst_Fpad + Slength - 1) := Source;

                  for I in Tfirst_Fpad + Slength .. Tlast loop
                     Target (I) := Pad;
                  end loop;
               end;
         end case;
      end if;
   end Move;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String is
   begin
      if Position - 1 not in Source'First - 1 .. Source'Last then
         raise Index_Error;
      end if;

      declare
         Result_Length : constant Natural :=
           Integer'Max (Source'Length,
                        Position - Source'First + New_Item'Length);
         Front         : constant Integer := Position - Source'First;

      begin
         return Result : String (1 .. Result_Length)
           with Relaxed_Initialization
         do
            Result (1 .. Front) := Source (Source'First .. Position - 1);
            pragma Assert
              (Result (1 .. Position - Source'First)
               = Source (Source'First .. Position - 1));
            Result (Front + 1 .. Front + New_Item'Length) := New_Item;
            pragma Assert
              (Result
                 (Position - Source'First + 1
                  .. Position - Source'First + New_Item'Length)
               = New_Item);

            if Position <= Source'Last - New_Item'Length then
               Result (Front + New_Item'Length + 1 .. Result'Last) :=
                 Source (Position + New_Item'Length .. Source'Last);
            end if;

            pragma Assert
              (if Position <= Source'Last - New_Item'Length
               then
                  Result
                 (Position - Source'First + New_Item'Length + 1
                  .. Result'Last)
               = Source (Position + New_Item'Length .. Source'Last));
         end return;
      end;
   end Overwrite;

   procedure Overwrite
     (Source   : in out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right)
   is
   begin
      Move (Source => Overwrite (Source, Position, New_Item),
            Target => Source,
            Drop   => Drop);
   end Overwrite;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String is
   begin
      if Low - 1 > Source'Last or else High < Source'First - 1 then
         raise Index_Error;
      end if;

      if High >= Low then
         declare
            Front_Len : constant Integer :=
              Integer'Max (0, Low - Source'First);
            --  Length of prefix of Source copied to result

            Back_Len : constant Integer := Integer'Max (0, Source'Last - High);
            --  Length of suffix of Source copied to result

            Result_Length : constant Integer :=
              Front_Len + By'Length + Back_Len;
            --  Length of result

         begin
            return Result : String (1 .. Result_Length)
              with Relaxed_Initialization do
               Result (1 .. Front_Len) := Source (Source'First .. Low - 1);
               pragma Assert
                 (Result (1 .. Integer'Max (0, Low - Source'First))
                  = Source (Source'First .. Low - 1));
               Result (Front_Len + 1 .. Front_Len + By'Length) := By;

               if High < Source'Last then
                  Result (Front_Len + By'Length + 1 .. Result'Last) :=
                    Source (High + 1 .. Source'Last);
               end if;

               pragma Assert
                 (Result (1 .. Integer'Max (0, Low - Source'First))
                  = Source (Source'First .. Low - 1));
               pragma Assert
                 (Result
                    (Integer'Max (0, Low - Source'First) + 1
                     .. Integer'Max (0, Low - Source'First) + By'Length)
                  = By);
               pragma Assert
                 (if High < Source'Last
                  then
                     Result
                    (Integer'Max (0, Low - Source'First) + By'Length + 1
                     .. Result'Last)
                  = Source (High + 1 .. Source'Last));
            end return;
         end;
      else
         return Insert (Source, Before => Low, New_Item => By);
      end if;
   end Replace_Slice;

   procedure Replace_Slice
     (Source   : in out String;
      Low      : Positive;
      High     : Natural;
      By       : String;
      Drop     : Truncation := Error;
      Justify  : Alignment  := Left;
      Pad      : Character  := Space)
   is
   begin
      Move (Replace_Slice (Source, Low, High, By), Source, Drop, Justify, Pad);
   end Replace_Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      subtype Result_Type is String (1 .. Count);

   begin
      if Count = 0 then
         return "";

      elsif Count < Source'Length then
         return Result_Type (Source (Source'Last - Count + 1 .. Source'Last));

      --  Pad on left

      else
         return Result : Result_Type with Relaxed_Initialization do
            for J in 1 .. Count - Source'Length loop
               Result (J) := Pad;
               pragma Loop_Invariant
                 (for all K in 1 .. J =>
                    Result (K)'Initialized and then Result (K) = Pad);
            end loop;

            if Source'Length /= 0 then
               Result (Count - Source'Length + 1 .. Count) := Source;
            end if;
         end return;
      end if;
   end Tail;

   procedure Tail
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Tail (Source, Count, Pad),
            Target  => Source,
            Drop    => Error,
            Justify => Justify,
            Pad     => Pad);
   end Tail;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping) return String
   is
   begin
      return Result : String (1 .. Source'Length)
        with Relaxed_Initialization
      do
         for J in Source'Range loop
            Result (J - (Source'First - 1)) := Value (Mapping, Source (J));
            pragma Loop_Invariant
              (for all K in Source'First .. J =>
                 Result (K - (Source'First - 1))'Initialized);
            pragma Loop_Invariant
              (for all K in Source'First .. J =>
                 Result (K - (Source'First - 1)) =
                   Value (Mapping, Source (K)));
         end loop;
      end return;
   end Translate;

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping)
   is
   begin
      for J in Source'Range loop
         Source (J) := Value (Mapping, Source (J));
         pragma Loop_Invariant
           (for all K in Source'First .. J =>
              Source (K) = Value (Mapping, Source'Loop_Entry (K)));
      end loop;
   end Translate;

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping_Function) return String
   is
      pragma Unsuppress (Access_Check);
   begin
      return Result : String (1 .. Source'Length)
        with Relaxed_Initialization
      do
         for J in Source'Range loop
            Result (J - (Source'First - 1)) := Mapping.all (Source (J));
            pragma Loop_Invariant
              (for all K in Source'First .. J =>
                 Result (K - (Source'First - 1))'Initialized);
            pragma Loop_Invariant
              (for all K in Source'First .. J =>
                 Result (K - (Source'First - 1)) = Mapping (Source (K)));
         end loop;
      end return;
   end Translate;

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping_Function)
   is
      pragma Unsuppress (Access_Check);
   begin
      for J in Source'Range loop
         Source (J) := Mapping.all (Source (J));
         pragma Loop_Invariant
           (for all K in Source'First .. J =>
              Source (K) = Mapping (Source'Loop_Entry (K)));
      end loop;
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : String;
      Side   : Trim_End) return String
   is
      Empty_String : constant String (1 .. 0) := "";
      --  Without declaring the empty string as a separate string starting
      --  at 1, SPARK provers have trouble proving the postcondition.
   begin
      case Side is
         when Strings.Left =>
            declare
               Low : constant Natural := Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return Empty_String;
               end if;

               declare
                  subtype Result_Type is String (1 .. Source'Last - Low + 1);
               begin
                  return Result_Type (Source (Low .. Source'Last));
               end;
            end;

         when Strings.Right =>
            declare
               High : constant Natural := Index_Non_Blank (Source, Backward);
            begin
               --  All blanks case

               if High = 0 then
                  return Empty_String;
               end if;

               declare
                  subtype Result_Type is String (1 .. High - Source'First + 1);
               begin
                  return Result_Type (Source (Source'First .. High));
               end;
            end;

         when Strings.Both =>
            declare
               Low : constant Natural := Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return Empty_String;
               end if;

               declare
                  High : constant Natural :=
                    Index_Non_Blank (Source, Backward);
                  subtype Result_Type is String (1 .. High - Low + 1);
               begin
                  return Result_Type (Source (Low .. High));
               end;
            end;
      end case;
   end Trim;

   procedure Trim
     (Source  : in out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   is
   begin
      Move (Trim (Source, Side),
            Source,
            Justify => Justify,
            Pad => Pad);
   end Trim;

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String
   is
      High, Low : Integer;

   begin
      Low := Index (Source, Set => Left, Test => Outside, Going => Forward);

      --  Case where source comprises only characters in Left

      if Low = 0 then
         return "";
      end if;

      High := Index (Source, Set => Right, Test => Outside, Going => Backward);

      --  Case where source comprises only characters in Right

      if High = 0 then
         return "";
      end if;

      declare
         Result_Length : constant Integer := High - Low + 1;
         subtype Result_Type is String (1 .. Result_Length);

      begin
         return Result_Type (Source (Low .. High));
      end;
   end Trim;

   procedure Trim
     (Source  : in out String;
      Left    : Maps.Character_Set;
      Right   : Maps.Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space)
   is
   begin
      Move (Source  => Trim (Source, Left, Right),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Trim;

end Ada.Strings.Fixed;
