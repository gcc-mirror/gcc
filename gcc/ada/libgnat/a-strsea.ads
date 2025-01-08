------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . S T R I N G S . S E A R C H                    --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains the search functions from Ada.Strings.Fixed. They
--  are separated out because they are shared by Ada.Strings.Bounded and
--  Ada.Strings.Unbounded, and we don't want to drag in other irrelevant stuff
--  from Ada.Strings.Fixed when using the other two packages. Although user
--  programs should access these subprograms via one of the standard string
--  packages, we do not make this a private package, since ghost function
--  Match is used in the contracts of the standard string packages.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced
--  by setting the corresponding assertion policy to Ignore. Postconditions,
--  contract cases and ghost code should not be executed at runtime as well,
--  in order not to slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with Ada.Strings.Maps; use type Ada.Strings.Maps.Character_Mapping_Function;

package Ada.Strings.Search with
  SPARK_Mode,
  Always_Terminates
is
   pragma Preelaborate;

   --  The ghost function Match tells whether the slice of Source starting at
   --  From and of length Pattern'Length matches with Pattern with respect to
   --  Mapping. Pattern should be non-empty and the considered slice should be
   --  fully included in Source'Range.

   function Match
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function;
      From    : Integer) return Boolean
   is
      (for all K in Pattern'Range =>
         Pattern (K) = Mapping (Source (From + (K - Pattern'First))))
   with
     Ghost,
     Pre    => Mapping /= null
       and then Pattern'Length > 0
       and then Source'Length > 0
       and then From in Source'First .. Source'Last - (Pattern'Length - 1),
     Global => null;
   pragma Annotate (GNATprove, False_Positive,
                    "call via access-to-subprogram",
                    "function Mapping must always terminate");

   function Match
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping;
      From    : Integer) return Boolean
   is
      (for all K in Pattern'Range =>
         Pattern (K) =
           Ada.Strings.Maps.Value
             (Mapping, Source (From + (K - Pattern'First))))
   with
     Ghost,
     Pre    => Pattern'Length > 0
       and then Source'Length > 0
       and then From in Source'First .. Source'Last - (Pattern'Length - 1),
     Global => null;

   function Is_Identity
     (Mapping : Maps.Character_Mapping) return Boolean
   with
     Post   => (if Is_Identity'Result then
                (for all K in Character =>
                   Ada.Strings.Maps.Value (Mapping, K) = K)),
     Global => null;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre            => Pattern'Length > 0,

     Post           => Index'Result in 0 | Source'Range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'Length = 0 => Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'Length > 0
          and then
            (for some J in
               Source'First .. Source'Last - (Pattern'Length - 1) =>
                 Match (Source, Pattern, Mapping, J))
        =>

          --  The result is in the considered range of Source

          Index'Result in Source'First .. Source'Last - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Match (Source, Pattern, Mapping, Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'Range =>
                 (if (if Going = Forward
                      then J <= Index'Result - 1
                      else J - 1 in Index'Result
                                    .. Source'Last - Pattern'Length)
                  then not Match (Source, Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others => Index'Result = 0),
     Global         => null;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre            => Pattern'Length > 0 and then Mapping /= null,
     Post           => Index'Result in 0 | Source'Range,
     Contract_Cases =>

       --  If Source is the null string, then 0 is returned

       (Source'Length = 0 => Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'Length > 0 and then
        (for some J in Source'First .. Source'Last - (Pattern'Length - 1) =>
             Match (Source, Pattern, Mapping, J))
        =>

          --  The result is in the considered range of Source

          Index'Result in Source'First .. Source'Last - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Match (Source, Pattern, Mapping, Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'Range =>
                 (if (if Going = Forward
                      then J <= Index'Result - 1
                      else J - 1 in Index'Result
                                    .. Source'Last - Pattern'Length)
                  then not Match (Source, Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others => Index'Result = 0),
     Global         => null;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   with
     Post           => Index'Result in 0 | Source'Range,
     Contract_Cases =>

        --  If no character of Source satisfies the property Test on Set, then
        --  0 is returned.

       ((for all C of Source =>
           (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))
        =>
          Index'Result = 0,

        --  Otherwise, an index in the range of Source is returned

        others =>

          --  The result is in the range of Source

          Index'Result in Source'Range

            --  The character at the returned index satisfies the property
            --  Test on Set.

            and then (Test = Inside)
              = Ada.Strings.Maps.Is_In (Source (Index'Result), Set)

            --  The result is the smallest or largest index which satisfies
            --  the property, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'Range =>
                 (if J /= Index'Result
                       and then (J < Index'Result) = (Going = Forward)
                  then (Test = Inside)
                       /= Ada.Strings.Maps.Is_In (Source (J), Set)))),
     Global         => null;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre            => Pattern'Length > 0
       and then (if Source'Length > 0 then From in Source'Range),

     Post           => Index'Result in 0 | Source'Range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'Length = 0 => Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'Length > 0
          and then
            (for some J in
              (if Going = Forward then From else Source'First)
              .. (if Going = Forward then Source'Last else From)
               - (Pattern'Length - 1) =>
              Match (Source, Pattern, Mapping, J))
        =>

          --  The result is in the considered range of Source

          Index'Result in
            (if Going = Forward then From else Source'First)
            .. (if Going = Forward then Source'Last else From)
              - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Match (Source, Pattern, Mapping, Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'Range =>
                 (if (if Going = Forward
                      then J in From .. Index'Result - 1
                      else J - 1 in Index'Result
                                    .. From - Pattern'Length)
                  then not Match (Source, Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others => Index'Result = 0),
     Global         => null;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre            => Pattern'Length > 0
       and then Mapping /= null
       and then (if Source'Length > 0 then From in Source'Range),

     Post           => Index'Result in 0 | Source'Range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'Length = 0 => Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'Length > 0
          and then
            (for some J in
              (if Going = Forward then From else Source'First)
              .. (if Going = Forward then Source'Last else From)
               - (Pattern'Length - 1) =>
              Match (Source, Pattern, Mapping, J))
        =>

          --  The result is in the considered range of Source

          Index'Result in
            (if Going = Forward then From else Source'First)
            .. (if Going = Forward then Source'Last else From)
              - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Match (Source, Pattern, Mapping, Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backwards.

            and then
              (for all J in Source'Range =>
                 (if (if Going = Forward
                      then J in From .. Index'Result - 1
                      else J - 1 in Index'Result
                                    .. From - Pattern'Length)
                  then not Match (Source, Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others => Index'Result = 0),
     Global         => null;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   with
     Pre            => (if Source'Length > 0 then From in Source'Range),
     Post           => Index'Result in 0 | Source'Range,
     Contract_Cases =>

        --  If Source is the empty string, or no character of the considered
        --  slice of Source satisfies the property Test on Set, then 0 is
        --  returned.

        (Source'Length = 0
          or else
            (for all J in Source'Range =>
               (if J = From or else (J > From) = (Going = Forward) then
                  (Test = Inside) /= Ada.Strings.Maps.Is_In (Source (J), Set)))
        =>
          Index'Result = 0,

        --  Otherwise, an index in the considered range of Source is returned

        others =>

          --  The result is in the considered range of Source

          Index'Result in Source'Range
            and then (Index'Result = From
                       or else
                         (Index'Result > From) = (Going = Forward))

            --  The character at the returned index satisfies the property
            --  Test on Set

            and then
              (Test = Inside)
              = Ada.Strings.Maps.Is_In (Source (Index'Result), Set)

            --  The result is the smallest or largest index which satisfies
            --  the property, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'Range =>
                 (if J /= Index'Result
                    and then (J < Index'Result) = (Going = Forward)
                    and then (J = From
                                or else (J > From) = (Going = Forward))
                  then (Test = Inside)
                       /= Ada.Strings.Maps.Is_In (Source (J), Set)))),
     Global         => null;

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   with
     Post           => Index_Non_Blank'Result in 0 | Source'Range,
     Contract_Cases =>

        --  If all characters of Source are Space characters, then 0 is
        --  returned.

       ((for all C of Source => C = ' ') => Index_Non_Blank'Result = 0,

        --  Otherwise, a valid index is returned

        others                           =>

          --  The result is in the range of Source

          Index_Non_Blank'Result in Source'Range

            --  The character at the returned index is not a Space character

            and then Source (Index_Non_Blank'Result) /= ' '

            --  The result is the smallest or largest index which is not a
            --  Space character, respectively when Going = Forward and
            --  Going = Backward.

            and then
              (for all J in Source'Range =>
                 (if J /= Index_Non_Blank'Result
                       and then (J < Index_Non_Blank'Result)
                              = (Going = Forward)
                  then Source (J) = ' '))),
     Global         => null;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   with
     Pre            => (if Source'Length /= 0 then From in Source'Range),
     Post           => Index_Non_Blank'Result in 0 | Source'Range,
     Contract_Cases =>

        --  If Source is the null string, or all characters in the considered
        --  slice of Source are Space characters, then 0 is returned.

        (Source'Length = 0
          or else
            (for all J in Source'Range =>
               (if J = From or else (J > From) = (Going = Forward) then
                  Source (J) = ' '))
        =>
          Index_Non_Blank'Result = 0,

        --  Otherwise, a valid index is returned

        others =>

          --  The result is in the considered range of Source

          Index_Non_Blank'Result in Source'Range
            and then (Index_Non_Blank'Result = From
                        or else (Index_Non_Blank'Result > From)
                                = (Going = Forward))

            --  The character at the returned index is not a Space character

            and then Source (Index_Non_Blank'Result) /= ' '

            --  The result is the smallest or largest index which is not a
            --  Space character, respectively when Going = Forward and
            --  Going = Backward.

            and then
              (for all J in Source'Range =>
                 (if J /= Index_Non_Blank'Result
                       and then (J < Index_Non_Blank'Result)
                                = (Going = Forward)
                       and then (J = From or else (J > From)
                                = (Going = Forward))
                  then Source (J) = ' '))),
     Global         => null;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre    => Pattern'Length > 0,
     Global => null;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre    => Pattern'Length > 0 and then Mapping /= null,
     Global => null;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
   with
     Global => null;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Pre            => (if Source'Length /= 0 then From in Source'Range),
     Contract_Cases =>

        --  If Source is the empty string, or if no character of the considered
        --  slice of Source satisfies the property Test on Set, then First is
        --  set to From and Last is set to 0.

       (Source'Length = 0
         or else
           (for all C of Source (From .. Source'Last) =>
              (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))
        =>
          First = From and then Last = 0,

        --  Otherwise, First and Last are set to valid indexes

        others =>

          --  First and Last are in the considered range of Source

          First in From .. Source'Last
            and then Last in First .. Source'Last

            --  No character between From and First satisfies the property Test
            --  on Set.

            and then
              (for all C of Source (From .. First - 1) =>
                 (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))

            --  All characters between First and Last satisfy the property Test
            --  on Set.

            and then
              (for all C of Source (First .. Last) =>
                 (Test = Inside) = Ada.Strings.Maps.Is_In (C, Set))

            --  If Last is not Source'Last, then the character at position
            --  Last + 1 does not satify the property Test on Set.

            and then
              (if Last < Source'Last
               then (Test = Inside)
                 /= Ada.Strings.Maps.Is_In (Source (Last + 1), Set))),
     Global         => null;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Pre            => Source'First > 0,
     Contract_Cases =>

        --  If Source is the empty string, or if no character of Source
        --  satisfies the property Test on Set, then First is set to From
        --  and Last is set to 0.

       (Source'Length = 0
         or else
           (for all C of Source =>
              (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))
        =>
          First = Source'First and then Last = 0,

        --  Otherwise, First and Last are set to valid indexes

        others =>

          --  First and Last are in the considered range of Source

          First in Source'Range
            and then Last in First .. Source'Last

            --  No character before First satisfies the property Test on Set

            and then
              (for all C of Source (Source'First .. First - 1) =>
                 (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))

            --  All characters between First and Last satisfy the property Test
            --  on Set.

            and then
              (for all C of Source (First .. Last) =>
                 (Test = Inside) = Ada.Strings.Maps.Is_In (C, Set))

            --  If Last is not Source'Last, then the character at position
            --  Last + 1 does not satify the property Test on Set.

            and then
              (if Last < Source'Last
               then (Test = Inside)
                 /= Ada.Strings.Maps.Is_In (Source (Last + 1), Set))),
     Global => null;

end Ada.Strings.Search;
