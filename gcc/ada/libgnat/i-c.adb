------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         I N T E R F A C E S . C                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

package body Interfaces.C
  with SPARK_Mode
is

   --------------------
   -- C_Length_Ghost --
   --------------------

   function C_Length_Ghost (Item : char_array) return size_t is
   begin
      for J in Item'Range loop
         if Item (J) = nul then
            return J - Item'First;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= nul);
      end loop;

      raise Program_Error;
   end C_Length_Ghost;

   function C_Length_Ghost (Item : wchar_array) return size_t is
   begin
      for J in Item'Range loop
         if Item (J) = wide_nul then
            return J - Item'First;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= wide_nul);
      end loop;

      raise Program_Error;
   end C_Length_Ghost;

   function C_Length_Ghost (Item : char16_array) return size_t is
   begin
      for J in Item'Range loop
         if Item (J) = char16_nul then
            return J - Item'First;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= char16_nul);
      end loop;

      raise Program_Error;
   end C_Length_Ghost;

   function C_Length_Ghost (Item : char32_array) return size_t is
   begin
      for J in Item'Range loop
         if Item (J) = char32_nul then
            return J - Item'First;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= char32_nul);
      end loop;

      raise Program_Error;
   end C_Length_Ghost;

   -----------------------
   -- Is_Nul_Terminated --
   -----------------------

   --  Case of char_array

   function Is_Nul_Terminated (Item : char_array) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) = nul then
            return True;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= nul);
      end loop;

      return False;
   end Is_Nul_Terminated;

   --  Case of wchar_array

   function Is_Nul_Terminated (Item : wchar_array) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) = wide_nul then
            return True;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= wide_nul);
      end loop;

      return False;
   end Is_Nul_Terminated;

   --  Case of char16_array

   function Is_Nul_Terminated (Item : char16_array) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) = char16_nul then
            return True;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= char16_nul);
      end loop;

      return False;
   end Is_Nul_Terminated;

   --  Case of char32_array

   function Is_Nul_Terminated (Item : char32_array) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) = char32_nul then
            return True;
         end if;

         pragma Loop_Invariant
           (for all K in Item'First .. J => Item (K) /= char32_nul);
      end loop;

      return False;
   end Is_Nul_Terminated;

   ------------
   -- To_Ada --
   ------------

   --  Convert char to Character

   function To_Ada (Item : char) return Character is
   begin
      return Character'Val (char'Pos (Item));
   end To_Ada;

   --  Convert char_array to String (function form)

   function To_Ada
     (Item     : char_array;
      Trim_Nul : Boolean := True) return String
   is
      Count : Natural;
      From  : size_t;

   begin
      if Trim_Nul then
         From := Item'First;

         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                 Item (J) /= nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         pragma Assert (From = Item'First + C_Length_Ghost (Item));

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      declare
         Count_Cst : constant Natural := Count;
         R : String (1 .. Count_Cst) with Relaxed_Initialization;

      begin
         for J in R'Range loop
            R (J) := To_Ada (Item (size_t (J) - 1 + Item'First));

            pragma Loop_Invariant (for all K in 1 .. J => R (K)'Initialized);
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                R (K) = To_Ada (Item (size_t (K) - 1 + Item'First)));
         end loop;

         return R;
      end;
   end To_Ada;

   --  Convert char_array to String (procedure form)

   procedure To_Ada
     (Item     : char_array;
      Target   : out String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   is
      From : size_t;
      To   : Integer;

   begin
      if Trim_Nul then
         From := Item'First;
         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                Item (J) /= nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      if Count > Target'Length then
         raise Constraint_Error;

      else
         From := Item'First;
         To   := Target'First;

         for J in 1 .. Count loop
            Target (To) := Character (Item (From));

            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant (To = Target'First + (J - 1));
            pragma Loop_Invariant (From = Item'First + size_t (J - 1));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all K in Target'First .. To =>
                Target (K) =
                  To_Ada (Item (size_t (K - Target'First) + Item'First)));

            --  Avoid possible overflow when incrementing To in the last
            --  iteration of the loop.
            exit when J = Count;

            From := From + 1;
            To   := To + 1;
         end loop;
      end if;
   end To_Ada;

   --  Convert wchar_t to Wide_Character

   function To_Ada (Item : wchar_t) return Wide_Character is
   begin
      return Wide_Character (Item);
   end To_Ada;

   --  Convert wchar_array to Wide_String (function form)

   function To_Ada
     (Item     : wchar_array;
      Trim_Nul : Boolean := True) return Wide_String
   is
      Count : Natural;
      From  : size_t;

   begin
      if Trim_Nul then
         From := Item'First;

         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = wide_nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                 Item (J) /= wide_nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = wide_nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         pragma Assert (From = Item'First + C_Length_Ghost (Item));

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      declare
         Count_Cst : constant Natural := Count;
         R : Wide_String (1 .. Count_Cst) with Relaxed_Initialization;

      begin
         for J in R'Range loop
            R (J) := To_Ada (Item (size_t (J) - 1 + Item'First));

            pragma Loop_Invariant (for all K in 1 .. J => R (K)'Initialized);
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                R (K) = To_Ada (Item (size_t (K) - 1 + Item'First)));
         end loop;

         return R;
      end;
   end To_Ada;

   --  Convert wchar_array to Wide_String (procedure form)

   procedure To_Ada
     (Item     : wchar_array;
      Target   : out Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   is
      From : size_t;
      To   : Integer;

   begin
      if Trim_Nul then
         From := Item'First;
         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = wide_nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                Item (J) /= wide_nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = wide_nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      if Count > Target'Length then
         raise Constraint_Error;

      else
         From := Item'First;
         To   := Target'First;

         for J in 1 .. Count loop
            Target (To) := To_Ada (Item (From));

            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant (To = Target'First + (J - 1));
            pragma Loop_Invariant (From = Item'First + size_t (J - 1));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all K in Target'First .. To =>
                Target (K) =
                  To_Ada (Item (size_t (K - Target'First) + Item'First)));

            --  Avoid possible overflow when incrementing To in the last
            --  iteration of the loop.
            exit when J = Count;

            From := From + 1;
            To   := To + 1;
         end loop;
      end if;
   end To_Ada;

   --  Convert char16_t to Wide_Character

   function To_Ada (Item : char16_t) return Wide_Character is
   begin
      return Wide_Character'Val (char16_t'Pos (Item));
   end To_Ada;

   --  Convert char16_array to Wide_String (function form)

   function To_Ada
     (Item     : char16_array;
      Trim_Nul : Boolean := True) return Wide_String
   is
      Count : Natural;
      From  : size_t;

   begin
      if Trim_Nul then
         From := Item'First;

         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = char16_nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                 Item (J) /= char16_nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = char16_nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         pragma Assert (From = Item'First + C_Length_Ghost (Item));

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      declare
         Count_Cst : constant Natural := Count;
         R : Wide_String (1 .. Count_Cst) with Relaxed_Initialization;

      begin
         for J in R'Range loop
            R (J) := To_Ada (Item (size_t (J) - 1 + Item'First));

            pragma Loop_Invariant (for all K in 1 .. J => R (K)'Initialized);
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                R (K) = To_Ada (Item (size_t (K) - 1 + Item'First)));
         end loop;

         return R;
      end;
   end To_Ada;

   --  Convert char16_array to Wide_String (procedure form)

   procedure To_Ada
     (Item     : char16_array;
      Target   : out Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   is
      From : size_t;
      To   : Integer;

   begin
      if Trim_Nul then
         From := Item'First;
         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = char16_nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                Item (J) /= char16_nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = char16_nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      if Count > Target'Length then
         raise Constraint_Error;

      else
         From := Item'First;
         To   := Target'First;

         for J in 1 .. Count loop
            Target (To) := To_Ada (Item (From));

            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant (To = Target'First + (J - 1));
            pragma Loop_Invariant (From = Item'First + size_t (J - 1));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all K in Target'First .. To =>
                Target (K) =
                  To_Ada (Item (size_t (K - Target'First) + Item'First)));

            --  Avoid possible overflow when incrementing To in the last
            --  iteration of the loop.
            exit when J = Count;

            From := From + 1;
            To   := To + 1;
         end loop;
      end if;
   end To_Ada;

   --  Convert char32_t to Wide_Wide_Character

   function To_Ada (Item : char32_t) return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (char32_t'Pos (Item));
   end To_Ada;

   --  Convert char32_array to Wide_Wide_String (function form)

   function To_Ada
     (Item     : char32_array;
      Trim_Nul : Boolean := True) return Wide_Wide_String
   is
      Count : Natural;
      From  : size_t;

   begin
      if Trim_Nul then
         From := Item'First;

         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = char32_nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                 Item (J) /= char32_nul);
            pragma Loop_Invariant (From <= Item'First + C_Length_Ghost (Item));
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = char32_nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         pragma Assert (From = Item'First + C_Length_Ghost (Item));

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      declare
         Count_Cst : constant Natural := Count;
         R : Wide_Wide_String (1 .. Count_Cst) with Relaxed_Initialization;

      begin
         for J in R'Range loop
            R (J) := To_Ada (Item (size_t (J) - 1 + Item'First));

            pragma Loop_Invariant (for all K in 1 .. J => R (K)'Initialized);
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                R (K) = To_Ada (Item (size_t (K) - 1 + Item'First)));
         end loop;

         return R;
      end;
   end To_Ada;

   --  Convert char32_array to Wide_Wide_String (procedure form)

   procedure To_Ada
     (Item     : char32_array;
      Target   : out Wide_Wide_String;
      Count    : out Natural;
      Trim_Nul : Boolean := True)
   is
      From : size_t;
      To   : Integer;

   begin
      if Trim_Nul then
         From := Item'First;
         loop
            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant
              (for some J in From .. Item'Last => Item (J) = char32_nul);
            pragma Loop_Invariant
              (for all J in Item'First .. From when J /= From =>
                Item (J) /= char32_nul);
            pragma Loop_Variant (Increases => From);

            if From > Item'Last then
               raise Terminator_Error;
            elsif Item (From) = char32_nul then
               exit;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      if Count > Target'Length then
         raise Constraint_Error;

      else
         From := Item'First;
         To   := Target'First;

         for J in 1 .. Count loop
            Target (To) := To_Ada (Item (From));

            pragma Loop_Invariant (From in Item'Range);
            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant (To = Target'First + (J - 1));
            pragma Loop_Invariant (From = Item'First + size_t (J - 1));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all K in Target'First .. To =>
                Target (K) =
                  To_Ada (Item (size_t (K - Target'First) + Item'First)));

            --  Avoid possible overflow when incrementing To in the last
            --  iteration of the loop.
            exit when J = Count;

            From := From + 1;
            To   := To + 1;
         end loop;
      end if;
   end To_Ada;

   ----------
   -- To_C --
   ----------

   --  Convert Character to char

   function To_C (Item : Character) return char is
   begin
      return char'Val (Character'Pos (Item));
   end To_C;

   --  Convert String to char_array (function form)

   function To_C
     (Item       : String;
      Append_Nul : Boolean := True) return char_array
   is
   begin
      if Append_Nul then
         declare
            R : char_array (0 .. Item'Length) with Relaxed_Initialization;

         begin
            for J in Item'Range loop
               R (size_t (J - Item'First)) := To_C (Item (J));

               pragma Loop_Invariant
                 (for all K in 0 .. size_t (J - Item'First) =>
                    R (K)'Initialized);
               pragma Loop_Invariant
                 (for all K in Item'First .. J =>
                    R (size_t (K - Item'First)) = To_C (Item (K)));
            end loop;

            R (R'Last) := nul;

            pragma Assert
              (for all J in Item'Range =>
                 R (size_t (J - Item'First)) = To_C (Item (J)));

            return R;
         end;

      --  Append_Nul False

      else
         --  A nasty case, if the string is null, we must return a null
         --  char_array. The lower bound of this array is required to be zero
         --  (RM B.3(50)) but that is of course impossible given that size_t
         --  is unsigned. According to Ada 2005 AI-258, the result is to raise
         --  Constraint_Error. This is also the appropriate behavior in Ada 95,
         --  since nothing else makes sense.

         if Item'Length = 0 then
            raise Constraint_Error;

         --  Normal case

         else
            declare
               R : char_array (0 .. Item'Length - 1)
                 with Relaxed_Initialization;

            begin
               for J in Item'Range loop
                  R (size_t (J - Item'First)) := To_C (Item (J));

                  pragma Loop_Invariant
                    (for all K in 0 .. size_t (J - Item'First) =>
                       R (K)'Initialized);
                  pragma Loop_Invariant
                    (for all K in Item'First .. J =>
                       R (size_t (K - Item'First)) = To_C (Item (K)));
               end loop;

               return R;
            end;
         end if;
      end if;
   end To_C;

   --  Convert String to char_array (procedure form)

   procedure To_C
     (Item       : String;
      Target     : out char_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   is
      To : size_t;

   begin
      if Target'Length < Item'Length then
         raise Constraint_Error;

      else
         To := Target'First;
         for From in Item'Range loop
            Target (To) := char (Item (From));

            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant
              (To - Target'First = size_t (From - Item'First));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all J in Item'First .. From =>
                 Target (Target'First + size_t (J - Item'First)) =
                   To_C (Item (J)));

            To := To + 1;
         end loop;

         if Append_Nul then
            if To > Target'Last then
               raise Constraint_Error;
            else
               Target (To) := nul;
               Count := Item'Length + 1;
            end if;

         else
            Count := Item'Length;
         end if;
      end if;
   end To_C;

   --  Convert Wide_Character to wchar_t

   function To_C (Item : Wide_Character) return wchar_t is
   begin
      return wchar_t (Item);
   end To_C;

   --  Convert Wide_String to wchar_array (function form)

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return wchar_array
   is
   begin
      if Append_Nul then
         declare
            R : wchar_array (0 .. Item'Length) with Relaxed_Initialization;

         begin
            for J in Item'Range loop
               R (size_t (J - Item'First)) := To_C (Item (J));

               pragma Loop_Invariant
                 (for all K in 0 .. size_t (J - Item'First) =>
                    R (K)'Initialized);
               pragma Loop_Invariant
                 (for all K in Item'First .. J =>
                    R (size_t (K - Item'First)) = To_C (Item (K)));
            end loop;

            R (R'Last) := wide_nul;

            pragma Assert
              (for all J in Item'Range =>
                 R (size_t (J - Item'First)) = To_C (Item (J)));

            return R;
         end;

      else
         --  A nasty case, if the string is null, we must return a null
         --  wchar_array. The lower bound of this array is required to be zero
         --  (RM B.3(50)) but that is of course impossible given that size_t
         --  is unsigned. According to Ada 2005 AI-258, the result is to raise
         --  Constraint_Error. This is also the appropriate behavior in Ada 95,
         --  since nothing else makes sense.

         if Item'Length = 0 then
            raise Constraint_Error;

         else
            declare
               R : wchar_array (0 .. Item'Length - 1)
                 with Relaxed_Initialization;

            begin
               for J in Item'Range loop
                  R (size_t (J - Item'First)) := To_C (Item (J));

                  pragma Loop_Invariant
                    (for all K in 0 .. size_t (J - Item'First) =>
                       R (K)'Initialized);
                  pragma Loop_Invariant
                    (for all K in Item'First .. J =>
                       R (size_t (K - Item'First)) = To_C (Item (K)));
               end loop;

               return R;
            end;
         end if;
      end if;
   end To_C;

   --  Convert Wide_String to wchar_array (procedure form)

   procedure To_C
     (Item       : Wide_String;
      Target     : out wchar_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   is
      To : size_t;

   begin
      if Target'Length < Item'Length then
         raise Constraint_Error;

      else
         To := Target'First;
         for From in Item'Range loop
            Target (To) := To_C (Item (From));

            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant
              (To - Target'First = size_t (From - Item'First));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all J in Item'First .. From =>
                Target (Target'First + size_t (J - Item'First)) =
                  To_C (Item (J)));

            To := To + 1;
         end loop;

         pragma Assert
           (for all J in Item'Range =>
             Target (Target'First + size_t (J - Item'First)) =
               To_C (Item (J)));
         pragma Assert
           (if Item'Length /= 0 then
             Target (Target'First ..
                     Target'First + (Item'Length - 1))'Initialized);

         if Append_Nul then
            if To > Target'Last then
               raise Constraint_Error;
            else
               Target (To) := wide_nul;
               Count := Item'Length + 1;
            end if;

         else
            Count := Item'Length;
         end if;
      end if;
   end To_C;

   --  Convert Wide_Character to char16_t

   function To_C (Item : Wide_Character) return char16_t is
   begin
      return char16_t'Val (Wide_Character'Pos (Item));
   end To_C;

   --  Convert Wide_String to char16_array (function form)

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return char16_array
   is
   begin
      if Append_Nul then
         declare
            R : char16_array (0 .. Item'Length) with Relaxed_Initialization;

         begin
            for J in Item'Range loop
               R (size_t (J - Item'First)) := To_C (Item (J));

               pragma Loop_Invariant
                 (for all K in 0 .. size_t (J - Item'First) =>
                    R (K)'Initialized);
               pragma Loop_Invariant
                 (for all K in Item'First .. J =>
                    R (size_t (K - Item'First)) = To_C (Item (K)));
            end loop;

            R (R'Last) := char16_nul;

            pragma Assert
              (for all J in Item'Range =>
                 R (size_t (J - Item'First)) = To_C (Item (J)));

            return R;
         end;

      else
         --  A nasty case, if the string is null, we must return a null
         --  char16_array. The lower bound of this array is required to be zero
         --  (RM B.3(50)) but that is of course impossible given that size_t
         --  is unsigned. According to Ada 2005 AI-258, the result is to raise
         --  Constraint_Error. This is also the appropriate behavior in Ada 95,
         --  since nothing else makes sense.

         if Item'Length = 0 then
            raise Constraint_Error;

         else
            declare
               R : char16_array (0 .. Item'Length - 1)
                 with Relaxed_Initialization;

            begin
               for J in Item'Range loop
                  R (size_t (J - Item'First)) := To_C (Item (J));

                  pragma Loop_Invariant
                    (for all K in 0 .. size_t (J - Item'First) =>
                       R (K)'Initialized);
                  pragma Loop_Invariant
                    (for all K in Item'First .. J =>
                       R (size_t (K - Item'First)) = To_C (Item (K)));
               end loop;

               return R;
            end;
         end if;
      end if;
   end To_C;

   --  Convert Wide_String to char16_array (procedure form)

   procedure To_C
     (Item       : Wide_String;
      Target     : out char16_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   is
      To : size_t;

   begin
      if Target'Length < Item'Length then
         raise Constraint_Error;

      else
         To := Target'First;
         for From in Item'Range loop
            Target (To) := To_C (Item (From));

            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant
              (To - Target'First = size_t (From - Item'First));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all J in Item'First .. From =>
                Target (Target'First + size_t (J - Item'First)) =
                  To_C (Item (J)));

            To := To + 1;
         end loop;

         pragma Assert
           (for all J in Item'Range =>
             Target (Target'First + size_t (J - Item'First)) =
               To_C (Item (J)));
         pragma Assert
           (if Item'Length /= 0 then
             Target (Target'First ..
                     Target'First + (Item'Length - 1))'Initialized);

         if Append_Nul then
            if To > Target'Last then
               raise Constraint_Error;
            else
               Target (To) := char16_nul;
               Count := Item'Length + 1;
            end if;

         else
            Count := Item'Length;
         end if;
      end if;
   end To_C;

   --  Convert Wide_Character to char32_t

   function To_C (Item : Wide_Wide_Character) return char32_t is
   begin
      return char32_t'Val (Wide_Wide_Character'Pos (Item));
   end To_C;

   --  Convert Wide_Wide_String to char32_array (function form)

   function To_C
     (Item       : Wide_Wide_String;
      Append_Nul : Boolean := True) return char32_array
   is
   begin
      if Append_Nul then
         declare
            R : char32_array (0 .. Item'Length) with Relaxed_Initialization;

         begin
            for J in Item'Range loop
               R (size_t (J - Item'First)) := To_C (Item (J));

               pragma Loop_Invariant
                 (for all K in 0 .. size_t (J - Item'First) =>
                    R (K)'Initialized);
               pragma Loop_Invariant
                 (for all K in Item'First .. J =>
                    R (size_t (K - Item'First)) = To_C (Item (K)));
            end loop;

            R (R'Last) := char32_nul;

            pragma Assert
              (for all J in Item'Range =>
                 R (size_t (J - Item'First)) = To_C (Item (J)));

            return R;
         end;

      else
         --  A nasty case, if the string is null, we must return a null
         --  char32_array. The lower bound of this array is required to be zero
         --  (RM B.3(50)) but that is of course impossible given that size_t
         --  is unsigned. According to Ada 2005 AI-258, the result is to raise
         --  Constraint_Error.

         if Item'Length = 0 then
            raise Constraint_Error;

         else
            declare
               R : char32_array (0 .. Item'Length - 1)
                 with Relaxed_Initialization;

            begin
               for J in Item'Range loop
                  R (size_t (J - Item'First)) := To_C (Item (J));

                  pragma Loop_Invariant
                    (for all K in 0 .. size_t (J - Item'First) =>
                       R (K)'Initialized);
                  pragma Loop_Invariant
                    (for all K in Item'First .. J =>
                       R (size_t (K - Item'First)) = To_C (Item (K)));
               end loop;

               return R;
            end;
         end if;
      end if;
   end To_C;

   --  Convert Wide_Wide_String to char32_array (procedure form)

   procedure To_C
     (Item       : Wide_Wide_String;
      Target     : out char32_array;
      Count      : out size_t;
      Append_Nul : Boolean := True)
   is
      To : size_t;

   begin
      if Target'Length < Item'Length + (if Append_Nul then 1 else 0) then
         raise Constraint_Error;

      else
         To := Target'First;
         for From in Item'Range loop
            Target (To) := To_C (Item (From));

            pragma Loop_Invariant (To in Target'Range);
            pragma Loop_Invariant
              (To - Target'First = size_t (From - Item'First));
            pragma Loop_Invariant
              (for all J in Target'First .. To => Target (J)'Initialized);
            pragma Loop_Invariant
              (Target (Target'First .. To)'Initialized);
            pragma Loop_Invariant
              (for all J in Item'First .. From =>
                Target (Target'First + size_t (J - Item'First)) =
                  To_C (Item (J)));

            To := To + 1;
         end loop;

         pragma Assert
           (for all J in Item'Range =>
             Target (Target'First + size_t (J - Item'First)) =
               To_C (Item (J)));
         pragma Assert
           (if Item'Length /= 0 then
             Target (Target'First ..
                     Target'First + (Item'Length - 1))'Initialized);

         if Append_Nul then
            Target (To) := char32_nul;
            Count := Item'Length + 1;
         else
            Count := Item'Length;
         end if;
      end if;
   end To_C;
   pragma Annotate (CodePeer, False_Positive, "validity check",
     "Count is only uninitialized on abnormal return.");

end Interfaces.C;
