------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . S T R I N G S . S U P E R B O U N D E D              --
--                                                                          --
--                                 S p e c                                  --
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

--  This non generic package contains most of the implementation of the
--  generic package Ada.Strings.Bounded.Generic_Bounded_Length.

--  It defines type Super_String as a discriminated record with the maximum
--  length as the discriminant. Individual instantiations of Strings.Bounded
--  use this type with an appropriate discriminant value set.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with Ada.Strings.Maps; use type Ada.Strings.Maps.Character_Mapping_Function;
with Ada.Strings.Search;
with Ada.Strings.Text_Buffers;

package Ada.Strings.Superbounded with SPARK_Mode is
   pragma Preelaborate;

   --  Type Bounded_String in Ada.Strings.Bounded.Generic_Bounded_Length is
   --  derived from Super_String, with the constraint of the maximum length.

   type Super_String_Data is new String with Relaxed_Initialization;

   type Super_String (Max_Length : Positive) is record
      Current_Length : Natural := 0;
      Data           : Super_String_Data (1 .. Max_Length);
      --  A previous version had a default initial value for Data, which is
      --  no longer necessary, because we now special-case this type in the
      --  compiler, so "=" composes properly for descendants of this type.
      --  Leaving it out is more efficient.
   end record
   with
     Predicate =>
       Current_Length <= Max_Length
         and then Data (1 .. Current_Length)'Initialized,
     Put_Image => Put_Image;

   --  The subprograms defined for Super_String are similar to those
   --  defined for Bounded_String, except that they have different names, so
   --  that they can be renamed in Ada.Strings.Bounded.Generic_Bounded_Length.

   function Super_Length (Source : Super_String) return Natural
   is (Source.Current_Length)
   with Global => null;

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Super_String
     (Source     : String;
      Max_Length : Positive;
      Drop       : Truncation := Error) return Super_String
   with
     Pre            => (if Source'Length > Max_Length then Drop /= Error),
     Post           => To_Super_String'Result.Max_Length = Max_Length,
     Contract_Cases =>
       (Source'Length <= Max_Length
        =>
          Super_To_String (To_Super_String'Result) = Source,

        Source'Length > Max_Length and then Drop = Left
        =>
          Super_To_String (To_Super_String'Result) =
            Source (Source'Last - Max_Length + 1 .. Source'Last),

        others  --  Drop = Right
        =>
          Super_To_String (To_Super_String'Result) =
            Source (Source'First .. Source'First - 1 + Max_Length)),
     Global         => null;
   --  Note the additional parameter Max_Length, which specifies the maximum
   --  length setting of the resulting Super_String value.

   --  The following procedures have declarations (and semantics) that are
   --  exactly analogous to those declared in Ada.Strings.Bounded.

   function Super_To_String (Source : Super_String) return String
   is (String (Source.Data (1 .. Source.Current_Length)));

   procedure Set_Super_String
     (Target : out Super_String;
      Source : String;
      Drop   : Truncation := Error)
   with
     Pre            =>
       (if Source'Length > Target.Max_Length then Drop /= Error),
     Contract_Cases =>
       (Source'Length <= Target.Max_Length
        =>
          Super_To_String (Target) = Source,

        Source'Length > Target.Max_Length and then Drop = Left
        =>
          Super_To_String (Target) =
            Source (Source'Last - Target.Max_Length + 1 .. Source'Last),

        others  --  Drop = Right
        =>
          Super_To_String (Target) =
            Source (Source'First .. Source'First - 1 + Target.Max_Length)),
     Global         => null;

   function Super_Append
     (Left  : Super_String;
      Right : Super_String;
      Drop  : Truncation  := Error) return Super_String
   with
     Pre            =>
       Left.Max_Length = Right.Max_Length
         and then
           (if Super_Length (Left) > Left.Max_Length - Super_Length (Right)
            then Drop /= Error),
     Post           => Super_Append'Result.Max_Length = Left.Max_Length,
     Contract_Cases =>
       (Super_Length (Left) <= Left.Max_Length - Super_Length (Right)
        =>
          Super_Length (Super_Append'Result) =
            Super_Length (Left) + Super_Length (Right)
            and then
              Super_Slice (Super_Append'Result, 1, Super_Length (Left)) =
                Super_To_String (Left)
            and then
              (if Super_Length (Right) > 0 then
                 Super_Slice (Super_Append'Result,
                   Super_Length (Left) + 1,
                   Super_Length (Super_Append'Result)) =
                     Super_To_String (Right)),

        Super_Length (Left) > Left.Max_Length - Super_Length (Right)
          and then Drop = Strings.Left
        =>
          Super_Length (Super_Append'Result) = Left.Max_Length
            and then
              (if Super_Length (Right) < Left.Max_Length then
                 String'(Super_Slice (Super_Append'Result,
                   1, Left.Max_Length - Super_Length (Right))) =
                     Super_Slice (Left,
                       Super_Length (Left) - Left.Max_Length
                         + Super_Length (Right) + 1,
                       Super_Length (Left)))
            and then
              Super_Slice (Super_Append'Result,
                Left.Max_Length - Super_Length (Right) + 1, Left.Max_Length) =
                  Super_To_String (Right),

        others  --  Drop = Right
        =>
          Super_Length (Super_Append'Result) = Left.Max_Length
            and then
              Super_Slice (Super_Append'Result, 1, Super_Length (Left)) =
                Super_To_String (Left)
            and then
              (if Super_Length (Left) < Left.Max_Length then
                 String'(Super_Slice (Super_Append'Result,
                   Super_Length (Left) + 1, Left.Max_Length)) =
                     Super_Slice (Right,
                       1, Left.Max_Length - Super_Length (Left)))),
     Global         => null;

   function Super_Append
     (Left  : Super_String;
      Right : String;
      Drop  : Truncation := Error) return Super_String
   with
     Pre            =>
       (if Right'Length > Left.Max_Length - Super_Length (Left)
        then Drop /= Error),
     Post           => Super_Append'Result.Max_Length = Left.Max_Length,
     Contract_Cases =>
       (Super_Length (Left) <= Left.Max_Length - Right'Length
        =>
          Super_Length (Super_Append'Result) =
            Super_Length (Left) + Right'Length
            and then
              Super_Slice (Super_Append'Result, 1, Super_Length (Left)) =
                Super_To_String (Left)
            and then
              (if Right'Length > 0 then
                 Super_Slice (Super_Append'Result,
                   Super_Length (Left) + 1,
                   Super_Length (Super_Append'Result)) =
                     Right),

        Super_Length (Left) > Left.Max_Length - Right'Length
          and then Drop = Strings.Left
        =>
          Super_Length (Super_Append'Result) = Left.Max_Length
            and then
              (if Right'Length < Left.Max_Length then

                 --  The result is the end of Left followed by Right

                 String'(Super_Slice (Super_Append'Result,
                   1, Left.Max_Length - Right'Length)) =
                     Super_Slice (Left,
                       Super_Length (Left) - Left.Max_Length + Right'Length
                         + 1,
                       Super_Length (Left))
                   and then
                     Super_Slice (Super_Append'Result,
                       Left.Max_Length - Right'Length + 1, Left.Max_Length) =
                         Right
               else
                 --  The result is the last Max_Length characters of Right

                 Super_To_String (Super_Append'Result) =
                   Right (Right'Last - Left.Max_Length + 1 .. Right'Last)),

        others  --  Drop = Right
        =>
          Super_Length (Super_Append'Result) = Left.Max_Length
            and then
              Super_Slice (Super_Append'Result, 1, Super_Length (Left)) =
                Super_To_String (Left)
            and then
              (if Super_Length (Left) < Left.Max_Length then
                 Super_Slice (Super_Append'Result,
                   Super_Length (Left) + 1, Left.Max_Length) =
                     Right (Right'First
                       .. Left.Max_Length - Super_Length (Left)
                            - 1 + Right'First))),
     Global         => null;

   function Super_Append
     (Left  : String;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String
   with
     Pre            =>
       (if Left'Length > Right.Max_Length - Super_Length (Right)
        then Drop /= Error),
     Post           => Super_Append'Result.Max_Length = Right.Max_Length,
     Contract_Cases =>
       (Left'Length <= Right.Max_Length - Super_Length (Right)
        =>
          Super_Length (Super_Append'Result) =
            Left'Length + Super_Length (Right)
            and then Super_Slice (Super_Append'Result, 1, Left'Length) = Left
            and then
              (if Super_Length (Right) > 0 then
                 Super_Slice (Super_Append'Result,
                   Left'Length + 1, Super_Length (Super_Append'Result)) =
                     Super_To_String (Right)),

        Left'Length > Right.Max_Length - Super_Length (Right)
          and then Drop = Strings.Left
        =>
          Super_Length (Super_Append'Result) = Right.Max_Length
            and then
              (if Super_Length (Right) < Right.Max_Length then
                 Super_Slice (Super_Append'Result,
                   1, Right.Max_Length - Super_Length (Right)) =
                     Left
                       (Left'Last - Right.Max_Length + Super_Length (Right) + 1
                        .. Left'Last))
            and then
              Super_Slice (Super_Append'Result,
                Right.Max_Length - Super_Length (Right) + 1,
                Right.Max_Length) =
                  Super_To_String (Right),

        others  --  Drop = Right
        =>
          Super_Length (Super_Append'Result) = Right.Max_Length
            and then
              (if Left'Length < Right.Max_Length then

                 --  The result is Left followed by the beginning of Right

                 Super_Slice (Super_Append'Result, 1, Left'Length) = Left
                   and then
                     String'(Super_Slice (Super_Append'Result,
                       Left'Length + 1, Right.Max_Length)) =
                         Super_Slice (Right, 1, Right.Max_Length - Left'Length)
               else
                 --  The result is the first Max_Length characters of Left

                 Super_To_String (Super_Append'Result) =
                   Left (Left'First .. Right.Max_Length - 1 + Left'First))),
     Global         => null;

   function Super_Append
     (Left  : Super_String;
      Right : Character;
      Drop  : Truncation := Error) return Super_String
   with
     Pre            =>
       (if Super_Length (Left) = Left.Max_Length then Drop /= Error),
     Post           => Super_Append'Result.Max_Length = Left.Max_Length,
     Contract_Cases =>
       (Super_Length (Left) < Left.Max_Length
        =>
          Super_Length (Super_Append'Result) = Super_Length (Left) + 1
            and then
              Super_Slice (Super_Append'Result, 1, Super_Length (Left)) =
                Super_To_String (Left)
            and then
              Super_Element (Super_Append'Result, Super_Length (Left) + 1) =
                Right,

        Super_Length (Left) = Left.Max_Length and then Drop = Strings.Right
        =>
          Super_Length (Super_Append'Result) = Left.Max_Length
            and then
              Super_To_String (Super_Append'Result) = Super_To_String (Left),

        others  --  Drop = Left
        =>
          Super_Length (Super_Append'Result) = Left.Max_Length
            and then
              String'(Super_Slice (Super_Append'Result,
                1, Left.Max_Length - 1)) =
                  Super_Slice (Left, 2, Left.Max_Length)
            and then
              Super_Element (Super_Append'Result, Left.Max_Length) = Right),
     Global         => null;

   function Super_Append
     (Left  : Character;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String
   with
     Pre            =>
       (if Super_Length (Right) = Right.Max_Length then Drop /= Error),
     Post           => Super_Append'Result.Max_Length = Right.Max_Length,
     Contract_Cases =>
       (Super_Length (Right) < Right.Max_Length
        =>
          Super_Length (Super_Append'Result) = Super_Length (Right) + 1
            and then
              Super_Slice (Super_Append'Result, 2, Super_Length (Right) + 1) =
                Super_To_String (Right)
            and then Super_Element (Super_Append'Result, 1) = Left,

        Super_Length (Right) = Right.Max_Length and then Drop = Strings.Left
        =>
          Super_Length (Super_Append'Result) = Right.Max_Length
            and then
              Super_To_String (Super_Append'Result) = Super_To_String (Right),

        others  --  Drop = Right
        =>
          Super_Length (Super_Append'Result) = Right.Max_Length
            and then
              String'(Super_Slice (Super_Append'Result, 2, Right.Max_Length)) =
                Super_Slice (Right, 1, Right.Max_Length - 1)
            and then Super_Element (Super_Append'Result, 1) = Left),
     Global         => null;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Super_String;
      Drop     : Truncation  := Error)
   with
     Pre            =>
       Source.Max_Length = New_Item.Max_Length
         and then
           (if Super_Length (Source) >
                 Source.Max_Length - Super_Length (New_Item)
            then Drop /= Error),
     Contract_Cases =>
       (Super_Length (Source) <= Source.Max_Length - Super_Length (New_Item)
        =>
          Super_Length (Source) =
            Super_Length (Source'Old) + Super_Length (New_Item)
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              (if Super_Length (New_Item) > 0 then
                 Super_Slice (Source,
                   Super_Length (Source'Old) + 1, Super_Length (Source)) =
                     Super_To_String (New_Item)),

        Super_Length (Source) > Source.Max_Length - Super_Length (New_Item)
          and then Drop = Left
        =>
          Super_Length (Source) = Source.Max_Length
            and then
              (if Super_Length (New_Item) < Source.Max_Length then
                 String'(Super_Slice (Source,
                   1, Source.Max_Length - Super_Length (New_Item))) =
                     Super_Slice (Source'Old,
                       Super_Length (Source'Old) - Source.Max_Length
                         + Super_Length (New_Item) + 1,
                       Super_Length (Source'Old)))
            and then
              Super_Slice (Source,
                Source.Max_Length - Super_Length (New_Item) + 1,
                Source.Max_Length) =
                  Super_To_String (New_Item),

        others  --  Drop = Right
        =>
          Super_Length (Source) = Source.Max_Length
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              (if Super_Length (Source'Old) < Source.Max_Length then
                 String'(Super_Slice (Source,
                   Super_Length (Source'Old) + 1, Source.Max_Length)) =
                     Super_Slice (New_Item,
                       1, Source.Max_Length - Super_Length (Source'Old)))),
     Global         => null;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : String;
      Drop     : Truncation  := Error)
   with
     Pre            =>
       (if New_Item'Length > Source.Max_Length - Super_Length (Source)
        then Drop /= Error),
     Contract_Cases =>
       (Super_Length (Source) <= Source.Max_Length - New_Item'Length
        =>
          Super_Length (Source) = Super_Length (Source'Old) + New_Item'Length
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              (if New_Item'Length > 0 then
                 Super_Slice (Source,
                   Super_Length (Source'Old) + 1, Super_Length (Source)) =
                     New_Item),

        Super_Length (Source) > Source.Max_Length - New_Item'Length
          and then Drop = Left
        =>
          Super_Length (Source) = Source.Max_Length
            and then
              (if New_Item'Length < Source.Max_Length then

                 --  The result is the end of Source followed by New_Item

                 String'(Super_Slice (Source,
                   1, Source.Max_Length - New_Item'Length)) =
                     Super_Slice (Source'Old,
                       Super_Length (Source'Old) - Source.Max_Length
                         + New_Item'Length + 1,
                       Super_Length (Source'Old))
                 and then
                   Super_Slice (Source,
                     Source.Max_Length - New_Item'Length + 1,
                     Source.Max_Length) =
                       New_Item
               else
                 --  The result is the last Max_Length characters of
                 --  New_Item.

                 Super_To_String (Source) = New_Item
                   (New_Item'Last - Source.Max_Length + 1 .. New_Item'Last)),

        others  --  Drop = Right
        =>
          Super_Length (Source) = Source.Max_Length
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              (if Super_Length (Source'Old) < Source.Max_Length then
                 Super_Slice (Source,
                   Super_Length (Source'Old) + 1, Source.Max_Length) =
                     New_Item (New_Item'First
                       .. Source.Max_Length - Super_Length (Source'Old) - 1
                         + New_Item'First))),
     Global         => null;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Character;
      Drop     : Truncation  := Error)
   with
     Pre            =>
       (if Super_Length (Source) = Source.Max_Length then Drop /= Error),
     Contract_Cases =>
       (Super_Length (Source) < Source.Max_Length
        =>
          Super_Length (Source) = Super_Length (Source'Old) + 1
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              Super_Element (Source, Super_Length (Source'Old) + 1) = New_Item,

        Super_Length (Source) = Source.Max_Length and then Drop = Right
        =>
          Super_Length (Source) = Source.Max_Length
            and then Super_To_String (Source) = Super_To_String (Source'Old),

        others  --  Drop = Left
        =>
          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source, 1, Source.Max_Length - 1)) =
                Super_Slice (Source'Old, 2, Source.Max_Length)
            and then Super_Element (Source, Source.Max_Length) = New_Item),
     Global         => null;

   function Concat
     (Left  : Super_String;
      Right : Super_String) return Super_String
   with
     Pre    => Left.Max_Length = Right.Max_Length
       and then Super_Length (Left) <= Left.Max_Length - Super_Length (Right),
     Post   => Concat'Result.Max_Length = Left.Max_Length
       and then
         Super_Length (Concat'Result) =
           Super_Length (Left) + Super_Length (Right)
       and then
         Super_Slice (Concat'Result, 1, Super_Length (Left)) =
           Super_To_String (Left)
       and then
         (if Super_Length (Right) > 0 then
            Super_Slice (Concat'Result,
              Super_Length (Left) + 1, Super_Length (Concat'Result)) =
              Super_To_String (Right)),
     Global => null;

   function Concat
     (Left  : Super_String;
      Right : String) return Super_String
   with
     Pre    => Right'Length <= Left.Max_Length - Super_Length (Left),
     Post   => Concat'Result.Max_Length = Left.Max_Length
       and then
         Super_Length (Concat'Result) = Super_Length (Left) + Right'Length
       and then
         Super_Slice (Concat'Result, 1, Super_Length (Left)) =
           Super_To_String (Left)
       and then
         (if Right'Length > 0 then
            Super_Slice (Concat'Result,
              Super_Length (Left) + 1, Super_Length (Concat'Result)) =
                Right),
     Global => null;

   function Concat
     (Left  : String;
      Right : Super_String) return Super_String
   with
     Pre    => Left'Length <= Right.Max_Length - Super_Length (Right),
     Post   => Concat'Result.Max_Length = Right.Max_Length
       and then
         Super_Length (Concat'Result) = Left'Length + Super_Length (Right)
       and then Super_Slice (Concat'Result, 1, Left'Length) = Left
       and then
         (if Super_Length (Right) > 0 then
            Super_Slice (Concat'Result,
              Left'Length + 1, Super_Length (Concat'Result)) =
                Super_To_String (Right)),
     Global => null;

   function Concat
     (Left  : Super_String;
      Right : Character) return Super_String
   with
     Pre    => Super_Length (Left) < Left.Max_Length,
     Post   => Concat'Result.Max_Length = Left.Max_Length
       and then Super_Length (Concat'Result) = Super_Length (Left) + 1
       and then
         Super_Slice (Concat'Result, 1, Super_Length (Left)) =
           Super_To_String (Left)
       and then Super_Element (Concat'Result, Super_Length (Left) + 1) = Right,
     Global => null;

   function Concat
     (Left  : Character;
      Right : Super_String) return Super_String
   with
     Pre    => Super_Length (Right) < Right.Max_Length,
     Post   => Concat'Result.Max_Length = Right.Max_Length
       and then Super_Length (Concat'Result) = 1 + Super_Length (Right)
       and then Super_Element (Concat'Result, 1) = Left
       and then
         Super_Slice (Concat'Result, 2, Super_Length (Concat'Result)) =
           Super_To_String (Right),
     Global => null;

   function Super_Element
     (Source : Super_String;
      Index  : Positive) return Character
   is (if Index <= Source.Current_Length
       then Source.Data (Index)
       else raise Index_Error)
   with Pre    => Index <= Super_Length (Source),
        Global => null;

   procedure Super_Replace_Element
     (Source : in out Super_String;
      Index  : Positive;
      By     : Character)
   with
     Pre    => Index <= Super_Length (Source),
     Post   => Super_Length (Source) = Super_Length (Source'Old)
       and then
         (for all K in 1 .. Super_Length (Source) =>
            Super_Element (Source, K) =
              (if K = Index then By else Super_Element (Source'Old, K))),
     Global => null;

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return String
   is (if Low - 1 > Source.Current_Length or else High > Source.Current_Length

       --  Note: test of High > Length is in accordance with AI95-00128

       then raise Index_Error
       else
          --  Note: in this case, superflat bounds are not a problem, we just
          --  get the null string in accordance with normal Ada slice rules.

          String (Source.Data (Low .. High)))
   with Pre    => Low - 1 <= Super_Length (Source)
                    and then High <= Super_Length (Source),
        Global => null;

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return Super_String
    with
     Pre    =>
       Low - 1 <= Super_Length (Source) and then High <= Super_Length (Source),
     Post   => Super_Slice'Result.Max_Length = Source.Max_Length
       and then
         Super_To_String (Super_Slice'Result) =
           Super_Slice (Source, Low, High),
     Global => null;

   procedure Super_Slice
     (Source : Super_String;
      Target : out Super_String;
      Low    : Positive;
      High   : Natural)
   with
     Pre    => Source.Max_Length = Target.Max_Length
       and then Low - 1 <= Super_Length (Source)
       and then High <= Super_Length (Source),
     Post   => Super_To_String (Target) = Super_Slice (Source, Low, High),
     Global => null;

   function "="
     (Left  : Super_String;
      Right : Super_String) return Boolean
   with
     Pre    => Left.Max_Length = Right.Max_Length,
     Post   => "="'Result = (Super_To_String (Left) = Super_To_String (Right)),
     Global => null;

   function Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean renames "=";

   function Equal
     (Left  : Super_String;
      Right : String) return Boolean
   with
     Post   => Equal'Result = (Super_To_String (Left) = Right),
     Global => null;

   function Equal
     (Left  : String;
      Right : Super_String) return Boolean
   with
     Post   => Equal'Result = (Left = Super_To_String (Right)),
     Global => null;

   function Less
     (Left  : Super_String;
      Right : Super_String) return Boolean
   with
     Pre    => Left.Max_Length = Right.Max_Length,
     Post   =>
       Less'Result = (Super_To_String (Left) < Super_To_String (Right)),
     Global => null;

   function Less
     (Left  : Super_String;
      Right : String) return Boolean
   with
     Post   => Less'Result = (Super_To_String (Left) < Right),
     Global => null;

   function Less
     (Left  : String;
      Right : Super_String) return Boolean
   with
     Post   => Less'Result = (Left < Super_To_String (Right)),
     Global => null;

   function Less_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean
   with
     Pre    => Left.Max_Length = Right.Max_Length,
     Post   =>
       Less_Or_Equal'Result =
         (Super_To_String (Left) <= Super_To_String (Right)),
     Global => null;

   function Less_Or_Equal
     (Left  : Super_String;
      Right : String) return Boolean
   with
     Post   => Less_Or_Equal'Result = (Super_To_String (Left) <= Right),
     Global => null;

   function Less_Or_Equal
     (Left  : String;
      Right : Super_String) return Boolean
   with
     Post   => Less_Or_Equal'Result = (Left <= Super_To_String (Right)),
     Global => null;

   function Greater
     (Left  : Super_String;
      Right : Super_String) return Boolean
   with
     Pre    => Left.Max_Length = Right.Max_Length,
     Post   =>
       Greater'Result = (Super_To_String (Left) > Super_To_String (Right)),
     Global => null;

   function Greater
     (Left  : Super_String;
      Right : String) return Boolean
   with
     Post   => Greater'Result = (Super_To_String (Left) > Right),
     Global => null;

   function Greater
     (Left  : String;
      Right : Super_String) return Boolean
   with
     Post   => Greater'Result = (Left > Super_To_String (Right)),
     Global => null;

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean
   with
     Pre    => Left.Max_Length = Right.Max_Length,
     Post   =>
       Greater_Or_Equal'Result =
         (Super_To_String (Left) >= Super_To_String (Right)),
     Global => null;

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : String) return Boolean
   with
     Post   => Greater_Or_Equal'Result = (Super_To_String (Left) >= Right),
     Global => null;

   function Greater_Or_Equal
     (Left  : String;
      Right : Super_String) return Boolean
   with
     Post   => Greater_Or_Equal'Result = (Left >= Super_To_String (Right)),
     Global => null;

   ----------------------
   -- Search Functions --
   ----------------------

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre            => Pattern'Length > 0,
     Post           => Super_Index'Result <= Super_Length (Source),
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Super_Length (Source) = 0
        =>
          Super_Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Super_Length (Source) > 0
          and then
            (for some J in 1 .. Super_Length (Source) - (Pattern'Length - 1) =>
               Search.Match (Super_To_String (Source), Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Super_Index'Result in
            1 .. Super_Length (Source) - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Search.Match
              (Super_To_String (Source), Pattern, Mapping, Super_Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if (if Going = Forward
                      then J <= Super_Index'Result - 1
                      else J - 1 in Super_Index'Result
                                    .. Super_Length (Source) - Pattern'Length)
                  then not (Search.Match
                    (Super_To_String (Source), Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Super_Index'Result = 0),
     Global         => null;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre            => Pattern'Length /= 0 and then Mapping /= null,
     Post           => Super_Index'Result <= Super_Length (Source),
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Super_Length (Source) = 0
        =>
          Super_Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Super_Length (Source) > 0
          and then
            (for some J in 1 .. Super_Length (Source) - (Pattern'Length - 1) =>
               Search.Match (Super_To_String (Source), Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Super_Index'Result in
            1 .. Super_Length (Source) - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Search.Match
              (Super_To_String (Source), Pattern, Mapping, Super_Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if (if Going = Forward
                      then J <= Super_Index'Result - 1
                      else J - 1 in Super_Index'Result
                                    .. Super_Length (Source) - Pattern'Length)
                  then not (Search.Match
                    (Super_To_String (Source), Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Super_Index'Result = 0),
     Global         => null;

   function Super_Index
     (Source : Super_String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   with
     Post           => Super_Index'Result <= Super_Length (Source),
     Contract_Cases =>

        --  If no character of Source satisfies the property Test on Set,
        --  then 0 is returned.

       ((for all C of Super_To_String (Source) =>
           (Test = Inside) /= Maps.Is_In (C, Set))
        =>
          Super_Index'Result = 0,

        --  Otherwise, an index in the range of Source is returned

        others
        =>
          --  The result is in the range of Source

          Super_Index'Result in 1 .. Super_Length (Source)

            --  The character at the returned index satisfies the property
            --  Test on Set.

            and then
              (Test = Inside) =
                Maps.Is_In (Super_Element (Source, Super_Index'Result), Set)

            --  The result is the smallest or largest index which satisfies
            --  the property, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if J /= Super_Index'Result
                       and then (J < Super_Index'Result) = (Going = Forward)
                  then (Test = Inside)
                       /= Maps.Is_In (Super_Element (Source, J), Set)))),
     Global         => null;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre            =>
       (if Super_Length (Source) /= 0 then From <= Super_Length (Source))
         and then Pattern'Length /= 0,
     Post           => Super_Index'Result <= Super_Length (Source),
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Super_Length (Source) = 0
        =>
          Super_Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Super_Length (Source) > 0
          and then
            (for some J in
              (if Going = Forward then From else 1)
               .. (if Going = Forward then Super_Length (Source) else From)
                - (Pattern'Length - 1) =>
              Search.Match (Super_To_String (Source), Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Super_Index'Result in
            (if Going = Forward then From else 1)
            .. (if Going = Forward then Super_Length (Source) else From)
             - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Search.Match
              (Super_To_String (Source), Pattern, Mapping, Super_Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if (if Going = Forward
                      then J in From .. Super_Index'Result - 1
                      else J - 1 in Super_Index'Result
                                    .. From - Pattern'Length)
                  then not (Search.Match
                    (Super_To_String (Source), Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Super_Index'Result = 0),
     Global         => null;

   function Super_Index
     (Source  : Super_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre            =>
       (if Super_Length (Source) /= 0 then From <= Super_Length (Source))
         and then Pattern'Length /= 0
         and then Mapping /= null,
     Post           => Super_Index'Result <= Super_Length (Source),
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Super_Length (Source) = 0
        =>
          Super_Index'Result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Super_Length (Source) > 0
          and then
            (for some J in
              (if Going = Forward then From else 1)
               .. (if Going = Forward then Super_Length (Source) else From)
                - (Pattern'Length - 1) =>
              Search.Match (Super_To_String (Source), Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Super_Index'Result in
            (if Going = Forward then From else 1)
            .. (if Going = Forward then Super_Length (Source) else From)
             - (Pattern'Length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then Search.Match
              (Super_To_String (Source), Pattern, Mapping, Super_Index'Result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if (if Going = Forward
                      then J in From .. Super_Index'Result - 1
                      else J - 1 in Super_Index'Result
                                    .. From - Pattern'Length)
                  then not (Search.Match
                    (Super_To_String (Source), Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Super_Index'Result = 0),
     Global         => null;

   function Super_Index
     (Source : Super_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural
   with
     Pre            =>
       (if Super_Length (Source) /= 0 then From <= Super_Length (Source)),
     Post           => Super_Index'Result <= Super_Length (Source),
     Contract_Cases =>

        --  If Source is the empty string, or no character of the considered
        --  slice of Source satisfies the property Test on Set, then 0 is
        --  returned.

       (Super_Length (Source) = 0
          or else
            (for all J in 1 .. Super_Length (Source) =>
               (if J = From or else (J > From) = (Going = Forward) then
                  (Test = Inside) /=
                    Maps.Is_In (Super_Element (Source, J), Set)))
        =>
          Super_Index'Result = 0,

        --  Otherwise, an index in the considered range of Source is returned

        others
        =>
          --  The result is in the considered range of Source

          Super_Index'Result in 1 .. Super_Length (Source)
            and then
              (Super_Index'Result = From
                 or else (Super_Index'Result > From) = (Going = Forward))

            --  The character at the returned index satisfies the property
            --  Test on Set.

            and then
              (Test = Inside) =
                Maps.Is_In (Super_Element (Source, Super_Index'Result), Set)

            --  The result is the smallest or largest index which satisfies
            --  the property, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if J /= Super_Index'Result
                    and then (J < Super_Index'Result) = (Going = Forward)
                    and then (J = From
                                or else (J > From) = (Going = Forward))
                  then (Test = Inside)
                       /= Maps.Is_In (Super_Element (Source, J), Set)))),
     Global         => null;

   function Super_Index_Non_Blank
     (Source : Super_String;
      Going  : Direction := Forward) return Natural
   with
     Post           => Super_Index_Non_Blank'Result <= Super_Length (Source),
     Contract_Cases =>

        --  If all characters of Source are Space characters, then 0 is
        --  returned.

       ((for all C of Super_To_String (Source) => C = ' ')
        =>
          Super_Index_Non_Blank'Result = 0,

        --  Otherwise, an index in the range of Source is returned

        others
        =>
          --  The result is in the range of Source

          Super_Index_Non_Blank'Result in 1 .. Super_Length (Source)

            --  The character at the returned index is not a Space character

            and then
              Super_Element (Source, Super_Index_Non_Blank'Result) /= ' '

            --  The result is the smallest or largest index which is not a
            --  Space character, respectively when Going = Forward and Going
            --  = Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if J /= Super_Index_Non_Blank'Result
                       and then
                         (J < Super_Index_Non_Blank'Result) = (Going = Forward)
                  then Super_Element (Source, J) = ' '))),
     Global         => null;

   function Super_Index_Non_Blank
     (Source : Super_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   with
     Pre            =>
       (if Super_Length (Source) /= 0 then From <= Super_Length (Source)),
     Post           => Super_Index_Non_Blank'Result <= Super_Length (Source),
     Contract_Cases =>

        --  If Source is the empty string, or all characters of the
        --  considered slice of Source are Space characters, then 0
        --  is returned.

       (Super_Length (Source) = 0
          or else
            (for all J in 1 .. Super_Length (Source) =>
               (if J = From or else (J > From) = (Going = Forward) then
                  Super_Element (Source, J) = ' '))
        =>
          Super_Index_Non_Blank'Result = 0,

        --  Otherwise, an index in the considered range of Source is returned

        others
        =>
          --  The result is in the considered range of Source

          Super_Index_Non_Blank'Result in 1 .. Super_Length (Source)
            and then
              (Super_Index_Non_Blank'Result = From
                 or else
                   (Super_Index_Non_Blank'Result > From) = (Going = Forward))

            --  The character at the returned index is not a Space character

            and then
              Super_Element (Source, Super_Index_Non_Blank'Result) /= ' '

            --  The result is the smallest or largest index which isn't a
            --  Space character, respectively when Going = Forward and Going
            --  = Backward.

            and then
              (for all J in 1 .. Super_Length (Source) =>
                 (if J /= Super_Index_Non_Blank'Result
                    and then
                      (J < Super_Index_Non_Blank'Result) = (Going = Forward)
                    and then (J = From
                                or else (J > From) = (Going = Forward))
                  then Super_Element (Source, J) = ' '))),
     Global         => null;

   function Super_Count
     (Source  : Super_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre    => Pattern'Length /= 0,
     Global => null;

   function Super_Count
     (Source  : Super_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre    => Pattern'Length /= 0 and then Mapping /= null,
     Global => null;

   function Super_Count
     (Source : Super_String;
      Set    : Maps.Character_Set) return Natural
   with
     Global => null;

   procedure Super_Find_Token
     (Source : Super_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Pre            =>
       (if Super_Length (Source) /= 0 then From <= Super_Length (Source)),
     Contract_Cases =>

        --  If Source is the empty string, or if no character of the
        --  considered slice of Source satisfies the property Test on
        --  Set, then First is set to From and Last is set to 0.

       (Super_Length (Source) = 0
          or else
            (for all J in From .. Super_Length (Source) =>
               (Test = Inside) /= Maps.Is_In (Super_Element (Source, J), Set))
        =>
          First = From and then Last = 0,

        --  Otherwise, First and Last are set to valid indexes

        others
        =>
          --  First and Last are in the considered range of Source

          First in From .. Super_Length (Source)
            and then Last in First .. Super_Length (Source)

            --  No character between From and First satisfies the property
            --  Test on Set.

            and then
              (for all J in From .. First - 1 =>
                 (Test = Inside) /=
                   Maps.Is_In (Super_Element (Source, J), Set))

            --  All characters between First and Last satisfy the property
            --  Test on Set.

            and then
              (for all J in First .. Last =>
                 (Test = Inside) =
                   Maps.Is_In (Super_Element (Source, J), Set))

            --  If Last is not Source'Last, then the character at position
            --  Last + 1 does not satify the property Test on Set.

            and then
              (if Last < Super_Length (Source)
               then
                 (Test = Inside) /=
                   Maps.Is_In (Super_Element (Source, Last + 1), Set))),
     Global         => null;

   procedure Super_Find_Token
     (Source : Super_String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Contract_Cases =>

        --  If Source is the empty string, or if no character of the considered
        --  slice of Source satisfies the property Test on Set, then First is
        --  set to 1 and Last is set to 0.

       (Super_Length (Source) = 0
          or else
            (for all J in 1 .. Super_Length (Source) =>
               (Test = Inside) /= Maps.Is_In (Super_Element (Source, J), Set))
        =>
          First = 1 and then Last = 0,

        --  Otherwise, First and Last are set to valid indexes

        others
        =>
          --  First and Last are in the considered range of Source

          First in 1 .. Super_Length (Source)
            and then Last in First .. Super_Length (Source)

            --  No character between 1 and First satisfies the property Test on
            --  Set.

            and then
              (for all J in 1 .. First - 1 =>
                 (Test = Inside) /=
                   Maps.Is_In (Super_Element (Source, J), Set))

            --  All characters between First and Last satisfy the property
            --  Test on Set.

            and then
              (for all J in First .. Last =>
                 (Test = Inside) =
                   Maps.Is_In (Super_Element (Source, J), Set))

            --  If Last is not Source'Last, then the character at position
            --  Last + 1 does not satify the property Test on Set.

            and then
              (if Last < Super_Length (Source)
               then
                 (Test = Inside) /=
                   Maps.Is_In (Super_Element (Source, Last + 1), Set))),
     Global         => null;

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Super_Translate
     (Source  : Super_String;
      Mapping : Maps.Character_Mapping) return Super_String
   with
     Post   => Super_Translate'Result.Max_Length = Source.Max_Length
       and then Super_Length (Super_Translate'Result) = Super_Length (Source)
       and then
         (for all K in 1 .. Super_Length (Source) =>
            Super_Element (Super_Translate'Result, K) =
              Ada.Strings.Maps.Value (Mapping, Super_Element (Source, K))),
     Global => null;

   procedure Super_Translate
     (Source   : in out Super_String;
      Mapping  : Maps.Character_Mapping)
   with
     Post   => Super_Length (Source) = Super_Length (Source'Old)
       and then
         (for all K in 1 .. Super_Length (Source) =>
            Super_Element (Source, K) =
              Ada.Strings.Maps.Value (Mapping, Super_Element (Source'Old, K))),
     Global => null;

   function Super_Translate
     (Source  : Super_String;
      Mapping : Maps.Character_Mapping_Function) return Super_String
   with
     Pre    => Mapping /= null,
     Post   => Super_Translate'Result.Max_Length = Source.Max_Length
       and then Super_Length (Super_Translate'Result) = Super_Length (Source)
       and then
         (for all K in 1 .. Super_Length (Source) =>
            Super_Element (Super_Translate'Result, K) =
              Mapping (Super_Element (Source, K))),
     Global => null;

   procedure Super_Translate
     (Source  : in out Super_String;
      Mapping : Maps.Character_Mapping_Function)
   with
     Pre    => Mapping /= null,
     Post   => Super_Length (Source) = Super_Length (Source'Old)
       and then
         (for all K in 1 .. Super_Length (Source) =>
            Super_Element (Source, K) =
              Mapping (Super_Element (Source'Old, K))),
     Global => null;

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Super_Replace_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural;
      By     : String;
      Drop   : Truncation := Error) return Super_String
   with
     Pre            =>
       Low - 1 <= Super_Length (Source)
         and then
           (if Drop = Error
              then (if High >= Low
                      then Low - 1
                        <= Source.Max_Length - By'Length
                         - Integer'Max (Super_Length (Source) - High, 0)
                      else Super_Length (Source) <=
                             Source.Max_Length - By'Length)),
     Post           =>
       Super_Replace_Slice'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>
       (Low - 1 <= Source.Max_Length - By'Length - Integer'Max
          (Super_Length (Source) - Integer'Max (High, Low - 1), 0)
        =>
          --  Total length is lower than Max_Length: nothing is dropped

          --  Note that if High < Low, the insertion is done before Low, so in
          --  all cases the starting position of the slice of Source remaining
          --  after the replaced Slice is Integer'Max (High + 1, Low).

          Super_Length (Super_Replace_Slice'Result) =
            Low - 1 + By'Length + Integer'Max
              (Super_Length (Source) - Integer'Max (High, Low - 1), 0)
            and then
              String'(Super_Slice (Super_Replace_Slice'Result, 1, Low - 1)) =
                Super_Slice (Source, 1, Low - 1)
            and then
              Super_Slice (Super_Replace_Slice'Result,
                Low, Low - 1 + By'Length) = By
            and then
              (if Integer'Max (High, Low - 1) < Super_Length (Source) then
                 String'(Super_Slice (Super_Replace_Slice'Result,
                   Low + By'Length,
                   Super_Length (Super_Replace_Slice'Result))) =
                     Super_Slice (Source,
                       Integer'Max (High + 1, Low), Super_Length (Source))),

        Low - 1 > Source.Max_Length - By'Length - Integer'Max
          (Super_Length (Source) - Integer'Max (High, Low - 1), 0)
          and then Drop = Left
        =>
          --  Final_Super_Slice is the length of the slice of Source remaining
          --  after the replaced part.
          (declare
             Final_Super_Slice : constant Natural :=
               Integer'Max
                 (Super_Length (Source) - Integer'Max (High, Low - 1), 0);
           begin
             --  The result is of maximal length and ends by the last
             --  Final_Super_Slice characters of Source.

             Super_Length (Super_Replace_Slice'Result) = Source.Max_Length
               and then
                 (if Final_Super_Slice > 0 then
                    String'(Super_Slice (Super_Replace_Slice'Result,
                      Source.Max_Length - Final_Super_Slice + 1,
                      Source.Max_Length)) =
                        Super_Slice (Source,
                          Integer'Max (High + 1, Low), Super_Length (Source)))

               --  Depending on when we reach Max_Length, either the first
               --  part of Source is fully dropped and By is partly dropped,
               --  or By is fully added and the first part of Source is partly
               --  dropped.

               and then
                (if Source.Max_Length - Final_Super_Slice - By'Length <= 0 then

                   --  The first (possibly zero) characters of By are dropped

                   (if Final_Super_Slice < Source.Max_Length then
                      Super_Slice (Super_Replace_Slice'Result,
                        1, Source.Max_Length - Final_Super_Slice) =
                          By (By'Last - Source.Max_Length + Final_Super_Slice
                                + 1
                              .. By'Last))

                 else  --  By is added to the result

                   Super_Slice (Super_Replace_Slice'Result,
                     Source.Max_Length - Final_Super_Slice - By'Length + 1,
                     Source.Max_Length - Final_Super_Slice) =
                       By

                     --  The first characters of Source (1 .. Low - 1) are
                     --  dropped.

                     and then
                       String'(Super_Slice (Super_Replace_Slice'Result, 1,
                         Source.Max_Length - Final_Super_Slice - By'Length)) =
                           Super_Slice (Source,
                             Low - Source.Max_Length
                               + Final_Super_Slice + By'Length,
                             Low - 1))),

        others  --  Drop = Right
        =>
          --  The result is of maximal length and starts by the first Low - 1
          --  characters of Source.

          Super_Length (Super_Replace_Slice'Result) = Source.Max_Length
            and then
              String'(Super_Slice (Super_Replace_Slice'Result, 1, Low - 1)) =
                Super_Slice (Source, 1, Low - 1)

            --  Depending on when we reach Max_Length, either the last part
            --  of Source is fully dropped and By is partly dropped, or By
            --  is fully added and the last part of Source is partly dropped.

            and then
              (if Low - 1 >= Source.Max_Length - By'Length then

                 --  The last characters of By are dropped

                 Super_Slice (Super_Replace_Slice'Result,
                   Low, Source.Max_Length) =
                     By (By'First .. Source.Max_Length - Low + By'First)

               else  --  By is fully added

                 Super_Slice (Super_Replace_Slice'Result,
                   Low, Low + By'Length - 1) = By

                   --  Then Source starting from Natural'Max (High + 1, Low)
                   --  is added but the last characters are dropped.

                   and then
                     String'(Super_Slice (Super_Replace_Slice'Result,
                       Low + By'Length, Source.Max_Length)) =
                         Super_Slice (Source, Integer'Max (High + 1, Low),
                           Integer'Max (High + 1, Low) +
                             (Source.Max_Length - Low - By'Length)))),
     Global         => null;

   procedure Super_Replace_Slice
     (Source  : in out Super_String;
      Low     : Positive;
      High    : Natural;
      By      : String;
      Drop    : Truncation := Error)
   with
     Pre            =>
       Low - 1 <= Super_Length (Source)
         and then
           (if Drop = Error
              then (if High >= Low
                      then Low - 1
                        <= Source.Max_Length - By'Length
                         - Natural'Max (Super_Length (Source) - High, 0)
                      else Super_Length (Source) <=
                             Source.Max_Length - By'Length)),
     Contract_Cases =>
       (Low - 1 <= Source.Max_Length - By'Length - Integer'Max
          (Super_Length (Source) - Integer'Max (High, Low - 1), 0)
        =>
          --  Total length is lower than Max_Length: nothing is dropped

          --  Note that if High < Low, the insertion is done before Low, so in
          --  all cases the starting position of the slice of Source remaining
          --  after the replaced Slice is Integer'Max (High + 1, Low).

          Super_Length (Source) = Low - 1 + By'Length + Integer'Max
            (Super_Length (Source'Old) - Integer'Max (High, Low - 1), 0)
            and then
              String'(Super_Slice (Source, 1, Low - 1)) =
                Super_Slice (Source'Old, 1, Low - 1)
            and then Super_Slice (Source, Low, Low - 1 + By'Length) = By
            and then
              (if Integer'Max (High, Low - 1) < Super_Length (Source'Old) then
                 String'(Super_Slice (Source,
                   Low + By'Length, Super_Length (Source))) =
                     Super_Slice (Source'Old,
                       Integer'Max (High + 1, Low),
                       Super_Length (Source'Old))),

        Low - 1 > Source.Max_Length - By'Length - Integer'Max
          (Super_Length (Source) - Integer'Max (High, Low - 1), 0)
          and then Drop = Left
        =>
          --  Final_Super_Slice is the length of the slice of Source remaining
          --  after the replaced part.
          (declare
             Final_Super_Slice : constant Natural :=
               Integer'Max (0,
                 Super_Length (Source'Old) - Integer'Max (High, Low - 1));
           begin
             --  The result is of maximal length and ends by the last
             --  Final_Super_Slice characters of Source.

             Super_Length (Source) = Source.Max_Length
               and then
                 (if Final_Super_Slice > 0 then
                    String'(Super_Slice (Source,
                      Source.Max_Length - Final_Super_Slice + 1,
                      Source.Max_Length)) =
                        Super_Slice (Source'Old,
                          Integer'Max (High + 1, Low),
                          Super_Length (Source'Old)))

               --  Depending on when we reach Max_Length, either the first
               --  part of Source is fully dropped and By is partly dropped,
               --  or By is fully added and the first part of Source is partly
               --  dropped.

               and then
                 (if Source.Max_Length - Final_Super_Slice - By'Length <= 0
                  then
                    --  The first characters of By are dropped

                    (if Final_Super_Slice < Source.Max_Length then
                       Super_Slice (Source,
                         1, Source.Max_Length - Final_Super_Slice) =
                           By (By'Last - Source.Max_Length + Final_Super_Slice
                                 + 1
                               .. By'Last))

                  else  --  By is added to the result

                    Super_Slice (Source,
                      Source.Max_Length - Final_Super_Slice - By'Length + 1,
                      Source.Max_Length - Final_Super_Slice) = By

                      --  The first characters of Source (1 .. Low - 1) are
                      --  dropped.

                      and then
                        String'(Super_Slice (Source, 1,
                          Source.Max_Length - Final_Super_Slice - By'Length)) =
                            Super_Slice (Source'Old,
                              Low - Source.Max_Length + Final_Super_Slice
                                + By'Length,
                              Low - 1))),

        others  --  Drop = Right
        =>
          --  The result is of maximal length and starts by the first Low - 1
          --  characters of Source.

          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source, 1, Low - 1)) =
                Super_Slice (Source'Old, 1, Low - 1)

            --  Depending on when we reach Max_Length, either the last part
            --  of Source is fully dropped and By is partly dropped, or By
            --  is fully added and the last part of Source is partly dropped.

            and then
              (if Low - 1 >= Source.Max_Length - By'Length then

                 --  The last characters of By are dropped

                 Super_Slice (Source, Low, Source.Max_Length) =
                   By (By'First .. Source.Max_Length - Low + By'First)

               else  --  By is fully added

                 Super_Slice (Source, Low, Low + By'Length - 1) = By

                   --  Then Source starting from Natural'Max (High + 1, Low)
                   --  is added but the last characters are dropped.

                   and then
                     String'(Super_Slice (Source,
                       Low + By'Length, Source.Max_Length)) =
                         Super_Slice (Source'Old, Integer'Max (High + 1, Low),
                           Integer'Max (High + 1, Low) +
                             (Source.Max_Length - Low - By'Length)))),
     Global         => null;

   function Super_Insert
     (Source   : Super_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error) return Super_String
   with
     Pre            =>
       Before - 1 <= Super_Length (Source)
         and then
           (if New_Item'Length > Source.Max_Length - Super_Length (Source)
            then Drop /= Error),
     Post           => Super_Insert'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>
       (Super_Length (Source) <= Source.Max_Length - New_Item'Length
        =>
          --  Total length is lower than Max_Length: nothing is dropped

          Super_Length (Super_Insert'Result) =
            Super_Length (Source) + New_Item'Length
            and then
              String'(Super_Slice (Super_Insert'Result, 1, Before - 1)) =
                Super_Slice (Source, 1, Before - 1)
            and then
              Super_Slice (Super_Insert'Result,
                Before, Before - 1 + New_Item'Length) =
                  New_Item
            and then
              (if Before <= Super_Length (Source) then
                 String'(Super_Slice (Super_Insert'Result,
                   Before + New_Item'Length,
                   Super_Length (Super_Insert'Result))) =
                     Super_Slice (Source, Before, Super_Length (Source))),

        Super_Length (Source) > Source.Max_Length - New_Item'Length
          and then Drop = Left
        =>
          --  The result is of maximal length and ends by the last characters
          --  of Source.

          Super_Length (Super_Insert'Result) = Source.Max_Length
            and then
              (if Before <= Super_Length (Source) then
                 String'(Super_Slice (Super_Insert'Result,
                   Source.Max_Length - Super_Length (Source) + Before,
                   Source.Max_Length)) =
                     Super_Slice (Source, Before, Super_Length (Source)))

            --  Depending on when we reach Max_Length, either the first part
            --  of Source is fully dropped and New_Item is partly dropped, or
            --  New_Item is fully added and the first part of Source is partly
            --  dropped.

            and then
              (if Source.Max_Length - Super_Length (Source) - 1 + Before
                < New_Item'Length
               then
                 --  The first characters of New_Item are dropped

                 (if Super_Length (Source) - Before + 1 < Source.Max_Length
                  then
                    Super_Slice (Super_Insert'Result, 1,
                      Source.Max_Length - Super_Length (Source) - 1 + Before) =
                        New_Item
                          (New_Item'Last - Source.Max_Length
                            + Super_Length (Source) - Before + 2
                           .. New_Item'Last))

               else  --  New_Item is added to the result

                 Super_Slice (Super_Insert'Result,
                   Source.Max_Length - Super_Length (Source) - New_Item'Length
                     + Before,
                   Source.Max_Length - Super_Length (Source) - 1 + Before) =
                     New_Item

                   --  The first characters of Source (1 .. Before - 1) are
                   --  dropped.

                   and then
                     String'(Super_Slice (Super_Insert'Result,
                       1, Source.Max_Length - Super_Length (Source)
                         - New_Item'Length - 1 + Before)) =
                           Super_Slice (Source,
                             Super_Length (Source) - Source.Max_Length
                               + New_Item'Length + 1,
                             Before - 1)),

        others  --  Drop = Right
        =>
          --  The result is of maximal length and starts by the first
          --  characters of Source.

          Super_Length (Super_Insert'Result) = Source.Max_Length
            and then
              String'(Super_Slice (Super_Insert'Result, 1, Before - 1)) =
                Super_Slice (Source, 1, Before - 1)

            --  Depending on when we reach Max_Length, either the last part
            --  of Source is fully dropped and New_Item is partly dropped, or
            --  New_Item is fully added and the last part of Source is partly
            --  dropped.

            and then
              (if Before - 1 >= Source.Max_Length - New_Item'Length then

                 --  The last characters of New_Item are dropped

                 Super_Slice (Super_Insert'Result, Before, Source.Max_Length) =
                   New_Item (New_Item'First
                     .. Source.Max_Length - Before + New_Item'First)

               else  --  New_Item is fully added

                 Super_Slice (Super_Insert'Result,
                   Before, Before + New_Item'Length - 1) =
                     New_Item

                   --  Then Source starting from Before is added but the
                   --  last characters are dropped.

                   and then
                     String'(Super_Slice (Super_Insert'Result,
                       Before + New_Item'Length, Source.Max_Length)) =
                         Super_Slice (Source,
                           Before, Source.Max_Length - New_Item'Length))),
     Global         => null;

   procedure Super_Insert
     (Source   : in out Super_String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   with
     Pre            =>
       Before - 1 <= Super_Length (Source)
         and then
           (if New_Item'Length > Source.Max_Length - Super_Length (Source)
            then Drop /= Error),
     Contract_Cases =>
       (Super_Length (Source) <= Source.Max_Length - New_Item'Length
        =>
          --  Total length is lower than Max_Length: nothing is dropped

          Super_Length (Source) = Super_Length (Source'Old) + New_Item'Length
            and then
              String'(Super_Slice (Source, 1, Before - 1)) =
                Super_Slice (Source'Old, 1, Before - 1)
            and then
              Super_Slice (Source, Before, Before - 1 + New_Item'Length) =
                New_Item
            and then
              (if Before <= Super_Length (Source'Old) then
                 String'(Super_Slice (Source,
                   Before + New_Item'Length, Super_Length (Source))) =
                     Super_Slice (Source'Old,
                       Before, Super_Length (Source'Old))),

        Super_Length (Source) > Source.Max_Length - New_Item'Length
          and then Drop = Left
        =>
          --  The result is of maximal length and ends by the last characters
          --  of Source.

          Super_Length (Source) = Source.Max_Length
            and then
              (if Before <= Super_Length (Source'Old) then
                 String'(Super_Slice (Source,
                   Source.Max_Length - Super_Length (Source'Old) + Before,
                   Source.Max_Length)) =
                     Super_Slice (Source'Old,
                       Before, Super_Length (Source'Old)))

            --  Depending on when we reach Max_Length, either the first part
            --  of Source is fully dropped and New_Item is partly dropped, or
            --  New_Item is fully added and the first part of Source is partly
            --  dropped.

            and then
              (if Source.Max_Length - Super_Length (Source'Old) - 1 + Before
                < New_Item'Length
               then
                 --  The first characters of New_Item are dropped

                 (if Super_Length (Source'Old) - Before + 1 < Source.Max_Length
                  then
                    Super_Slice (Source,
                      1, Source.Max_Length - Super_Length (Source'Old)
                        - 1 + Before) =
                          New_Item
                            (New_Item'Last - Source.Max_Length
                               + Super_Length (Source'Old) - Before + 2
                             .. New_Item'Last))

               else  --  New_Item is added to the result

                 Super_Slice (Source,
                   Source.Max_Length - Super_Length (Source'Old)
                     - New_Item'Length + Before,
                   Source.Max_Length - Super_Length (Source'Old) - 1 + Before)
                     = New_Item

                   --  The first characters of Source (1 .. Before - 1) are
                   --  dropped.

                   and then
                     String'(Super_Slice (Source, 1,
                       Source.Max_Length - Super_Length (Source'Old)
                         - New_Item'Length - 1 + Before)) =
                           Super_Slice (Source'Old,
                             Super_Length (Source'Old)
                               - Source.Max_Length + New_Item'Length + 1,
                             Before - 1)),

        others  --  Drop = Right
        =>
          --  The result is of maximal length and starts by the first
          --  characters of Source.

          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source, 1, Before - 1)) =
                Super_Slice (Source'Old, 1, Before - 1)

            --  Depending on when we reach Max_Length, either the last part
            --  of Source is fully dropped and New_Item is partly dropped, or
            --  New_Item is fully added and the last part of Source is partly
            --  dropped.

            and then
              (if Before - 1 >= Source.Max_Length - New_Item'Length then

                 --  The last characters of New_Item are dropped

                 Super_Slice (Source, Before, Source.Max_Length) =
                   New_Item (New_Item'First
                     .. Source.Max_Length - Before + New_Item'First)

               else  --  New_Item is fully added

                 Super_Slice (Source, Before, Before + New_Item'Length - 1) =
                   New_Item

                   --  Then Source starting from Before is added but the
                   --  last characters are dropped.

                   and then
                     String'(Super_Slice (Source,
                       Before + New_Item'Length, Source.Max_Length)) =
                         Super_Slice (Source'Old,
                           Before, Source.Max_Length - New_Item'Length))),
     Global         => null;

   function Super_Overwrite
     (Source   : Super_String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Error) return Super_String
   with
     Pre            =>
       Position - 1 <= Super_Length (Source)
         and then (if New_Item'Length > Source.Max_Length - (Position - 1)
                   then Drop /= Error),
     Post           => Super_Overwrite'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>
       (Position - 1 <= Source.Max_Length - New_Item'Length
        =>
          --  The length is unchanged, unless New_Item overwrites further than
          --  the end of Source. In this contract case, we suppose New_Item
          --  doesn't overwrite further than Max_Length.

          Super_Length (Super_Overwrite'Result) =
            Integer'Max (Super_Length (Source), Position - 1 + New_Item'Length)
            and then
              String'(Super_Slice (Super_Overwrite'Result, 1, Position - 1)) =
                Super_Slice (Source, 1, Position - 1)
            and then Super_Slice (Super_Overwrite'Result,
              Position, Position - 1 + New_Item'Length) =
                New_Item
            and then
              (if Position - 1 + New_Item'Length < Super_Length (Source) then

                 --  There are some unchanged characters of Source remaining
                 --  after New_Item.

                 String'(Super_Slice (Super_Overwrite'Result,
                   Position + New_Item'Length, Super_Length (Source))) =
                     Super_Slice (Source,
                       Position + New_Item'Length, Super_Length (Source))),

        Position - 1 > Source.Max_Length - New_Item'Length and then Drop = Left
        =>
          Super_Length (Super_Overwrite'Result) = Source.Max_Length

            --  If a part of the result has to be dropped, it means New_Item is
            --  overwriting further than the end of Source. Thus the result is
            --  necessarily ending by New_Item. However, we don't know whether
            --  New_Item covers all Max_Length characters or some characters of
            --  Source are remaining at the left.

            and then
              (if New_Item'Length >= Source.Max_Length then

                 --  New_Item covers all Max_Length characters

                 Super_To_String (Super_Overwrite'Result) =
                   New_Item
                     (New_Item'Last - Source.Max_Length + 1 .. New_Item'Last)
               else
                 --  New_Item fully appears at the end

                 Super_Slice (Super_Overwrite'Result,
                   Source.Max_Length - New_Item'Length + 1,
                   Source.Max_Length) =
                     New_Item

                   --  The left of Source is cut

                   and then
                     String'(Super_Slice (Super_Overwrite'Result,
                       1, Source.Max_Length - New_Item'Length)) =
                         Super_Slice (Source,
                           Position - Source.Max_Length + New_Item'Length,
                           Position - 1)),

        others  --  Drop = Right
        =>
          --  The result is of maximal length and starts by the first
          --  characters of Source.

          Super_Length (Super_Overwrite'Result) = Source.Max_Length
            and then
              String'(Super_Slice (Super_Overwrite'Result, 1, Position - 1)) =
                Super_Slice (Source, 1, Position - 1)

            --  Then New_Item is written until Max_Length

            and then Super_Slice (Super_Overwrite'Result,
              Position, Source.Max_Length) =
                New_Item (New_Item'First
                          .. Source.Max_Length - Position + New_Item'First)),
     Global         => null;

   procedure Super_Overwrite
     (Source    : in out Super_String;
      Position  : Positive;
      New_Item  : String;
      Drop      : Truncation := Error)
   with
     Pre            =>
       Position - 1 <= Super_Length (Source)
         and then (if New_Item'Length > Source.Max_Length - (Position - 1)
                   then Drop /= Error),
     Contract_Cases =>
       (Position - 1 <= Source.Max_Length - New_Item'Length
        =>
          --  The length is unchanged, unless New_Item overwrites further than
          --  the end of Source. In this contract case, we suppose New_Item
          --  doesn't overwrite further than Max_Length.

          Super_Length (Source) = Integer'Max
            (Super_Length (Source'Old), Position - 1 + New_Item'Length)
            and then
              String'(Super_Slice (Source, 1, Position - 1)) =
                Super_Slice (Source'Old, 1, Position - 1)
            and then Super_Slice (Source,
              Position, Position - 1 + New_Item'Length) =
                New_Item
            and then
              (if Position - 1 + New_Item'Length < Super_Length (Source'Old)
               then
                 --  There are some unchanged characters of Source remaining
                 --  after New_Item.

                 String'(Super_Slice (Source,
                   Position + New_Item'Length, Super_Length (Source'Old))) =
                     Super_Slice (Source'Old,
                       Position + New_Item'Length, Super_Length (Source'Old))),

        Position - 1 > Source.Max_Length - New_Item'Length and then Drop = Left
        =>
          Super_Length (Source) = Source.Max_Length

            --  If a part of the result has to be dropped, it means New_Item is
            --  overwriting further than the end of Source. Thus the result is
            --  necessarily ending by New_Item. However, we don't know whether
            --  New_Item covers all Max_Length characters or some characters of
            --  Source are remaining at the left.

            and then
              (if New_Item'Length >= Source.Max_Length then

                 --  New_Item covers all Max_Length characters

                 Super_To_String (Source) =
                   New_Item
                     (New_Item'Last - Source.Max_Length + 1 .. New_Item'Last)
               else
                 --  New_Item fully appears at the end

                 Super_Slice (Source,
                   Source.Max_Length - New_Item'Length + 1,
                   Source.Max_Length) =
                     New_Item

                   --  The left of Source is cut

                   and then
                     String'(Super_Slice (Source,
                       1, Source.Max_Length - New_Item'Length)) =
                         Super_Slice (Source'Old,
                           Position - Source.Max_Length + New_Item'Length,
                           Position - 1)),

        others  --  Drop = Right
        =>
          --  The result is of maximal length and starts by the first
          --  characters of Source.

          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source, 1, Position - 1)) =
                Super_Slice (Source'Old, 1, Position - 1)

            --  New_Item is written until Max_Length

            and then Super_Slice (Source, Position, Source.Max_Length) =
              New_Item (New_Item'First
                        .. Source.Max_Length - Position + New_Item'First)),
     Global         => null;

   function Super_Delete
     (Source  : Super_String;
      From    : Positive;
      Through : Natural) return Super_String
   with
     Pre            =>
       (if Through >= From then From - 1 <= Super_Length (Source)),
     Post           => Super_Delete'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>
       (Through >= From =>
          Super_Length (Super_Delete'Result) =
            From - 1 + Natural'Max (Super_Length (Source) - Through, 0)
            and then
              String'(Super_Slice (Super_Delete'Result, 1, From - 1)) =
                Super_Slice (Source, 1, From - 1)
            and then
              (if Through < Super_Length (Source) then
                 String'(Super_Slice (Super_Delete'Result,
                   From, Super_Length (Super_Delete'Result))) =
                     Super_Slice (Source, Through + 1, Super_Length (Source))),
        others          =>
          Super_Delete'Result = Source),
     Global         => null;

   procedure Super_Delete
     (Source  : in out Super_String;
      From    : Positive;
      Through : Natural)
   with
     Pre            =>
       (if Through >= From then From - 1 <= Super_Length (Source)),
     Contract_Cases =>
       (Through >= From =>
          Super_Length (Source) =
            From - 1 + Natural'Max (Super_Length (Source'Old) - Through, 0)
            and then
              String'(Super_Slice (Source, 1, From - 1)) =
                Super_Slice (Source'Old, 1, From - 1)
            and then
              (if Through < Super_Length (Source) then
                 String'(Super_Slice (Source, From, Super_Length (Source))) =
                   Super_Slice (Source'Old,
                     Through + 1, Super_Length (Source'Old))),
        others          =>
          Source = Source'Old),
     Global         => null;

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Super_Trim
     (Source : Super_String;
      Side   : Trim_End) return Super_String
   with
     Post           => Super_Trim'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>

       --  If all characters in Source are Space, the returned string is empty

       ((for all C of Super_To_String (Source) => C = ' ')
        =>
          Super_Length (Super_Trim'Result) = 0,

        --  Otherwise, the returned string is a slice of Source

        others
        =>
          (declare
             Low  : constant Positive :=
               (if Side = Right then 1
                else Super_Index_Non_Blank (Source, Forward));
             High : constant Positive :=
               (if Side = Left then Super_Length (Source)
                else Super_Index_Non_Blank (Source, Backward));
           begin
             Super_To_String (Super_Trim'Result) =
               Super_Slice (Source, Low, High))),
     Global         => null;

   procedure Super_Trim
     (Source : in out Super_String;
      Side   : Trim_End)
   with
     Contract_Cases =>

       --  If all characters in Source are Space, the returned string is empty

       ((for all C of Super_To_String (Source) => C = ' ')
        =>
          Super_Length (Source) = 0,

        --  Otherwise, the returned string is a slice of Source

        others
        =>
          (declare
             Low  : constant Positive :=
               (if Side = Right then 1
                else Super_Index_Non_Blank (Source'Old, Forward));
             High : constant Positive :=
               (if Side = Left then Super_Length (Source'Old)
                else Super_Index_Non_Blank (Source'Old, Backward));
           begin
             Super_To_String (Source) = Super_Slice (Source'Old, Low, High))),
     Global => null;

   function Super_Trim
     (Source : Super_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Super_String
   with
     Post           => Super_Trim'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>

       --  If all characters in Source are contained in one of the sets Left or
       --  Right, then the returned string is empty.

       ((for all C of Super_To_String (Source) => Maps.Is_In (C, Left))
          or else
            (for all C of Super_To_String (Source) => Maps.Is_In (C, Right))
        =>
          Super_Length (Super_Trim'Result) = 0,

        --  Otherwise, the returned string is a slice of Source

        others
        =>
          (declare
             Low  : constant Positive :=
               Super_Index (Source, Left, Outside, Forward);
             High : constant Positive :=
               Super_Index (Source, Right, Outside, Backward);
           begin
             Super_To_String (Super_Trim'Result) =
               Super_Slice (Source, Low, High))),
     Global         => null;

   procedure Super_Trim
     (Source : in out Super_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set)
   with
     Contract_Cases =>

       --  If all characters in Source are contained in one of the sets Left or
       --  Right, then the returned string is empty.

       ((for all C of Super_To_String (Source) => Maps.Is_In (C, Left))
          or else
            (for all C of Super_To_String (Source) => Maps.Is_In (C, Right))
        =>
          Super_Length (Source) = 0,

        --  Otherwise, the returned string is a slice of Source

        others
        =>
          (declare
             Low  : constant Positive :=
               Super_Index (Source'Old, Left, Outside, Forward);
             High : constant Positive :=
               Super_Index (Source'Old, Right, Outside, Backward);
           begin
             Super_To_String (Source) = Super_Slice (Source'Old, Low, High))),
     Global => null;

   function Super_Head
     (Source : Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error) return Super_String
   with
     Pre            => (if Count > Source.Max_Length then Drop /= Error),
     Post           => Super_Head'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>
       (Count <= Super_Length (Source)
        =>
          --  Source is cut

          Super_To_String (Super_Head'Result) = Super_Slice (Source, 1, Count),
        Count > Super_Length (Source) and then Count <= Source.Max_Length
        =>
          --  Source is followed by Pad characters

          Super_Length (Super_Head'Result) = Count
            and then
              Super_Slice (Super_Head'Result, 1, Super_Length (Source)) =
                Super_To_String (Source)
            and then
              String'(Super_Slice (Super_Head'Result,
                Super_Length (Source) + 1, Count)) =
                  [1 .. Count - Super_Length (Source) => Pad],
        Count > Source.Max_Length and then Drop = Right
        =>
          --  Source is followed by Pad characters

          Super_Length (Super_Head'Result) = Source.Max_Length
            and then
              Super_Slice (Super_Head'Result, 1, Super_Length (Source)) =
                Super_To_String (Source)
            and then
              String'(Super_Slice (Super_Head'Result,
                Super_Length (Source) + 1, Source.Max_Length)) =
                  [1 .. Source.Max_Length - Super_Length (Source) => Pad],
        Count - Super_Length (Source) > Source.Max_Length and then Drop = Left
        =>
          --  Source is fully dropped on the left

          Super_To_String (Super_Head'Result) =
            [1 .. Source.Max_Length => Pad],
        others
        =>
          --  Source is partly dropped on the left

          Super_Length (Super_Head'Result) = Source.Max_Length
            and then
              String'(Super_Slice (Super_Head'Result,
                1, Source.Max_Length - Count + Super_Length (Source))) =
                  Super_Slice (Source,
                    Count - Source.Max_Length + 1, Super_Length (Source))
            and then
              String'(Super_Slice (Super_Head'Result,
                Source.Max_Length - Count + Super_Length (Source) + 1,
                Source.Max_Length)) =
                  [1 .. Count - Super_Length (Source) => Pad]),
     Global         => null;

   procedure Super_Head
     (Source : in out Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error)
   with
     Pre            => (if Count > Source.Max_Length then Drop /= Error),
     Contract_Cases =>
       (Count <= Super_Length (Source)
        =>
          --  Source is cut

          Super_To_String (Source) = Super_Slice (Source'Old, 1, Count),
        Count > Super_Length (Source) and then Count <= Source.Max_Length
        =>
          --  Source is followed by Pad characters

          Super_Length (Source) = Count
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              String'(Super_Slice (Source,
                Super_Length (Source'Old) + 1, Count)) =
                  [1 .. Count - Super_Length (Source'Old) => Pad],
        Count > Source.Max_Length and then Drop = Right
        =>
          --  Source is followed by Pad characters

          Super_Length (Source) = Source.Max_Length
            and then
              Super_Slice (Source, 1, Super_Length (Source'Old)) =
                Super_To_String (Source'Old)
            and then
              String'(Super_Slice (Source,
                Super_Length (Source'Old) + 1, Source.Max_Length)) =
                  [1 .. Source.Max_Length - Super_Length (Source'Old) => Pad],
        Count - Super_Length (Source) > Source.Max_Length and then Drop = Left
        =>
          --  Source is fully dropped on the left

          Super_To_String (Source) = [1 .. Source.Max_Length => Pad],
        others
        =>
          --  Source is partly dropped on the left

          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source,
                1, Source.Max_Length - Count + Super_Length (Source'Old))) =
                  Super_Slice (Source'Old,
                    Count - Source.Max_Length + 1, Super_Length (Source'Old))
            and then
              String'(Super_Slice (Source,
                Source.Max_Length - Count + Super_Length (Source'Old) + 1,
                Source.Max_Length)) =
                  [1 .. Count - Super_Length (Source'Old) => Pad]),
     Global         => null;

   function Super_Tail
     (Source : Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error) return Super_String
   with
     Pre            => (if Count > Source.Max_Length then Drop /= Error),
     Post           => Super_Tail'Result.Max_Length = Source.Max_Length,
     Contract_Cases =>
       (Count < Super_Length (Source)
        =>
          --  Source is cut

          (if Count > 0 then
             Super_To_String (Super_Tail'Result) =
               Super_Slice (Source,
                 Super_Length (Source) - Count + 1, Super_Length (Source))
           else Super_Length (Super_Tail'Result) = 0),
        Count >= Super_Length (Source) and then Count < Source.Max_Length
        =>
          --  Source is preceded by Pad characters

          Super_Length (Super_Tail'Result) = Count
            and then
              String'(Super_Slice (Super_Tail'Result,
                1, Count - Super_Length (Source))) =
                  [1 .. Count - Super_Length (Source) => Pad]
            and then
              Super_Slice (Super_Tail'Result,
                Count - Super_Length (Source) + 1, Count) =
                  Super_To_String (Source),
        Count >= Source.Max_Length and then Drop = Left
        =>
          --  Source is preceded by Pad characters

          Super_Length (Super_Tail'Result) = Source.Max_Length
            and then
              String'(Super_Slice (Super_Tail'Result,
                1, Source.Max_Length - Super_Length (Source))) =
                  [1 .. Source.Max_Length - Super_Length (Source) => Pad]
            and then
              (if Super_Length (Source) > 0 then
                 Super_Slice (Super_Tail'Result,
                   Source.Max_Length - Super_Length (Source) + 1,
                   Source.Max_Length) =
                     Super_To_String (Source)),
        Count - Super_Length (Source) >= Source.Max_Length
          and then Drop /= Left
        =>
          --  Source is fully dropped on the right

          Super_To_String (Super_Tail'Result) =
            [1 .. Source.Max_Length => Pad],
        others
        =>
          --  Source is partly dropped on the right

          Super_Length (Super_Tail'Result) = Source.Max_Length
            and then
              String'(Super_Slice (Super_Tail'Result,
                1, Count - Super_Length (Source))) =
                  [1 .. Count - Super_Length (Source) => Pad]
            and then
              String'(Super_Slice (Super_Tail'Result,
                Count - Super_Length (Source) + 1, Source.Max_Length)) =
                  Super_Slice (Source,
                    1, Source.Max_Length - Count + Super_Length (Source))),
     Global         => null;

   procedure Super_Tail
     (Source : in out Super_String;
      Count  : Natural;
      Pad    : Character := Space;
      Drop   : Truncation := Error)
   with
     Pre            => (if Count > Source.Max_Length then Drop /= Error),
     Contract_Cases =>
       (Count < Super_Length (Source)
        =>
          --  Source is cut

          (if Count > 0 then
             Super_To_String (Source) =
               Super_Slice (Source'Old,
                 Super_Length (Source'Old) - Count + 1,
                 Super_Length (Source'Old))
           else Super_Length (Source) = 0),
        Count >= Super_Length (Source) and then Count < Source.Max_Length
        =>
          --  Source is preceded by Pad characters

          Super_Length (Source) = Count
            and then
              String'(Super_Slice (Source,
                1, Count - Super_Length (Source'Old))) =
                  [1 .. Count - Super_Length (Source'Old) => Pad]
            and then
              Super_Slice (Source,
                Count - Super_Length (Source'Old) + 1, Count) =
                  Super_To_String (Source'Old),
        Count >= Source.Max_Length and then Drop = Left
        =>
          --  Source is preceded by Pad characters

          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source,
                1, Source.Max_Length - Super_Length (Source'Old))) =
                  [1 .. Source.Max_Length - Super_Length (Source'Old) => Pad]
            and then
              (if Super_Length (Source'Old) > 0 then
                 Super_Slice (Source,
                   Source.Max_Length - Super_Length (Source'Old) + 1,
                   Source.Max_Length) =
                     Super_To_String (Source'Old)),
        Count - Super_Length (Source) >= Source.Max_Length
          and then Drop /= Left
        =>
          --  Source is fully dropped on the right

          Super_To_String (Source) = [1 .. Source.Max_Length => Pad],
        others
        =>
          --  Source is partly dropped on the right

          Super_Length (Source) = Source.Max_Length
            and then
              String'(Super_Slice (Source,
                1, Count - Super_Length (Source'Old))) =
                  [1 .. Count - Super_Length (Source'Old) => Pad]
            and then
              String'(Super_Slice (Source,
                Count - Super_Length (Source'Old) + 1, Source.Max_Length)) =
                  Super_Slice (Source'Old,
                    1, Source.Max_Length - Count + Super_Length (Source'Old))),
     Global         => null;

   ------------------------------------
   -- String Constructor Subprograms --
   ------------------------------------

   --  Note: in some of the following routines, there is an extra parameter
   --  Max_Length which specifies the value of the maximum length for the
   --  resulting Super_String value.

   function Times
     (Left       : Natural;
      Right      : Character;
      Max_Length : Positive) return Super_String
   with
     Pre    => Left <= Max_Length,
     Post   => Times'Result.Max_Length = Max_Length
       and then Super_To_String (Times'Result) = [1 .. Left => Right],
     Global => null;
   --  Note the additional parameter Max_Length

   function Times
     (Left       : Natural;
      Right      : String;
      Max_Length : Positive) return Super_String
   with
     Pre    => (if Left /= 0 then Right'Length <= Max_Length / Left),
     Post   => Times'Result.Max_Length = Max_Length
       and then Super_Length (Times'Result) = Left * Right'Length
       and then
         (if Right'Length > 0 then
            (for all K in 1 .. Left * Right'Length =>
               Super_Element (Times'Result, K) =
                 Right (Right'First + (K - 1) mod Right'Length))),
     Global => null;
   --  Note the additional parameter Max_Length

   function Times
     (Left  : Natural;
      Right : Super_String) return Super_String
   with
     Pre    =>
       (if Left /= 0 then Super_Length (Right) <= Right.Max_Length / Left),
     Post   => Times'Result.Max_Length = Right.Max_Length
       and then Super_Length (Times'Result) = Left * Super_Length (Right)
       and then
         (if Super_Length (Right) > 0 then
            (for all K in 1 .. Left * Super_Length (Right) =>
               Super_Element (Times'Result, K) =
                 Super_Element (Right, 1 + (K - 1) mod Super_Length (Right)))),
     Global => null;

   function Super_Replicate
     (Count      : Natural;
      Item       : Character;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String
   with
     Pre    => (if Count > Max_Length then Drop /= Error),
     Post   => Super_Replicate'Result.Max_Length = Max_Length
       and then Super_To_String (Super_Replicate'Result) =
         [1 .. Natural'Min (Max_Length, Count) => Item],
     Global => null;
   --  Note the additional parameter Max_Length

   function Super_Replicate
     (Count      : Natural;
      Item       : String;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String
   with
     Pre            =>
       (if Count /= 0 and then Item'Length > Max_Length / Count
        then Drop /= Error),
     Post           => Super_Replicate'Result.Max_Length = Max_Length,
     Contract_Cases =>
       (Count = 0 or else Item'Length <= Max_Length / Count
        =>
          Super_Length (Super_Replicate'Result) = Count * Item'Length
            and then
              (if Item'Length > 0 then
                 (for all K in 1 .. Count * Item'Length =>
                    Super_Element (Super_Replicate'Result, K) =
                      Item (Item'First + (K - 1) mod Item'Length))),
        Count /= 0
          and then Item'Length > Max_Length / Count
          and then Drop = Right
        =>
          Super_Length (Super_Replicate'Result) = Max_Length
            and then
              (for all K in 1 .. Max_Length =>
                 Super_Element (Super_Replicate'Result, K) =
                   Item (Item'First + (K - 1) mod Item'Length)),
        others  --  Drop = Left
        =>
          Super_Length (Super_Replicate'Result) = Max_Length
            and then
              (for all K in 1 .. Max_Length =>
                 Super_Element (Super_Replicate'Result, K) =
                   Item (Item'Last - (Max_Length - K) mod Item'Length))),
     Global         => null;
   --  Note the additional parameter Max_Length

   function Super_Replicate
     (Count : Natural;
      Item  : Super_String;
      Drop  : Truncation := Error) return Super_String
   with
     Pre            =>
       (if Count /= 0
          and then Super_Length (Item) > Item.Max_Length / Count
        then Drop /= Error),
     Post           => Super_Replicate'Result.Max_Length = Item.Max_Length,
     Contract_Cases =>
       ((if Count /= 0 then Super_Length (Item) <= Item.Max_Length / Count)
        =>
          Super_Length (Super_Replicate'Result) = Count * Super_Length (Item)
            and then
              (if Super_Length (Item) > 0 then
                 (for all K in 1 .. Count * Super_Length (Item) =>
                    Super_Element (Super_Replicate'Result, K) =
                      Super_Element (Item,
                        1 + (K - 1) mod Super_Length (Item)))),
        Count /= 0
          and then Super_Length (Item) > Item.Max_Length / Count
          and then Drop = Right
        =>
          Super_Length (Super_Replicate'Result) = Item.Max_Length
            and then
              (for all K in 1 .. Item.Max_Length =>
                 Super_Element (Super_Replicate'Result, K) =
                   Super_Element (Item, 1 + (K - 1) mod Super_Length (Item))),
        others  --  Drop = Left
        =>
          Super_Length (Super_Replicate'Result) = Item.Max_Length
            and then
              (for all K in 1 .. Item.Max_Length =>
                 Super_Element (Super_Replicate'Result, K) =
                   Super_Element (Item,
                     Super_Length (Item)
                       - (Item.Max_Length - K) mod Super_Length (Item)))),
     Global         => null;

   procedure Put_Image
     (S      : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Source : Super_String);

private
      --  Pragma Inline declarations

      pragma Inline ("=");
      pragma Inline (Less);
      pragma Inline (Less_Or_Equal);
      pragma Inline (Greater);
      pragma Inline (Greater_Or_Equal);
      pragma Inline (Concat);
      pragma Inline (Super_Count);
      pragma Inline (Super_Element);
      pragma Inline (Super_Find_Token);
      pragma Inline (Super_Index);
      pragma Inline (Super_Index_Non_Blank);
      pragma Inline (Super_Length);
      pragma Inline (Super_Replace_Element);
      pragma Inline (Super_Slice);
      pragma Inline (Super_To_String);

end Ada.Strings.Superbounded;
