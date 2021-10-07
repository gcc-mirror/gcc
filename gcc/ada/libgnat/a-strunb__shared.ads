------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . S T R I N G S . U N B O U N D E D                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore.

pragma Assertion_Policy (Pre => Ignore);

--  This package provides an implementation of Ada.Strings.Unbounded that uses
--  reference counts to implement copy on modification (rather than copy on
--  assignment). This is significantly more efficient on many targets.

--  This version is supported on:
--    - all Alpha platforms
--    - all ia64 platforms
--    - all PowerPC platforms
--    - all SPARC V9 platforms
--    - all x86 platforms
--    - all x86_64 platforms

   --  This package uses several techniques to increase speed:

   --   - Implicit sharing or copy-on-write. An Unbounded_String contains only
   --     the reference to the data which is shared between several instances.
   --     The shared data is reallocated only when its value is changed and
   --     the object mutation can't be used or it is inefficient to use it.

   --   - Object mutation. Shared data object can be reused without memory
   --     reallocation when all of the following requirements are met:
   --      - the shared data object is no longer used by anyone else;
   --      - the size is sufficient to store the new value;
   --      - the gap after reuse is less than a defined threshold.

   --   - Memory preallocation. Most of used memory allocation algorithms
   --     align allocated segments on the some boundary, thus some amount of
   --     additional memory can be preallocated without any impact. Such
   --     preallocated memory can used later by Append/Insert operations
   --     without reallocation.

   --  Reference counting uses GCC builtin atomic operations, which allows safe
   --  sharing of internal data between Ada tasks. Nevertheless, this does not
   --  make objects of Unbounded_String thread-safe: an instance cannot be
   --  accessed by several tasks simultaneously.

with Ada.Strings.Maps;
private with Ada.Finalization;
private with System.Atomic_Counters;
private with Ada.Strings.Text_Buffers;

package Ada.Strings.Unbounded with
  Initial_Condition => Length (Null_Unbounded_String) = 0
is
   pragma Preelaborate;

   type Unbounded_String is private with
     Default_Initial_Condition => Length (Unbounded_String) = 0;
   pragma Preelaborable_Initialization (Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String;

   function Length (Source : Unbounded_String) return Natural with
     Global => null;

   type String_Access is access all String;

   procedure Free (X : in out String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_String
     (Source : String)  return Unbounded_String
   with
     Post   => Length (To_Unbounded_String'Result) = Source'Length,
     Global => null;

   function To_Unbounded_String
     (Length : Natural) return Unbounded_String
   with
     Post   =>
       Ada.Strings.Unbounded.Length (To_Unbounded_String'Result) = Length,
     Global => null;

   function To_String (Source : Unbounded_String) return String with
     Post   => To_String'Result'Length = Length (Source),
     Global => null;

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String)
   with
     Global => null;
   pragma Ada_05 (Set_Unbounded_String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Unbounded_String)
   with
     Pre    => Length (New_Item) <= Natural'Last - Length (Source),
     Post   => Length (Source) = Length (Source)'Old + Length (New_Item),
     Global => null;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : String)
   with
     Pre    => New_Item'Length <= Natural'Last - Length (Source),
     Post   => Length (Source) = Length (Source)'Old + New_Item'Length,
     Global => null;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Character)
   with
     Pre    => Length (Source) < Natural'Last,
     Post   => Length (Source) = Length (Source)'Old + 1,
     Global => null;

   function "&"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => Length (Right) <= Natural'Last - Length (Left),
     Post   => Length ("&"'Result) = Length (Left) + Length (Right),
     Global => null;

   function "&"
     (Left  : Unbounded_String;
      Right : String) return Unbounded_String
   with
     Pre    => Right'Length <= Natural'Last - Length (Left),
     Post   => Length ("&"'Result) = Length (Left) + Right'Length,
     Global => null;

   function "&"
     (Left  : String;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => Left'Length <= Natural'Last - Length (Right),
     Post   => Length ("&"'Result) = Left'Length + Length (Right),
     Global => null;

   function "&"
     (Left  : Unbounded_String;
      Right : Character) return Unbounded_String
   with
     Pre    => Length (Left) < Natural'Last,
     Post   => Length ("&"'Result) = Length (Left) + 1,
     Global => null;

   function "&"
     (Left  : Character;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => Length (Right) < Natural'Last,
     Post   => Length ("&"'Result) = Length (Right) + 1,
     Global => null;

   function Element
     (Source : Unbounded_String;
      Index  : Positive) return Character
   with
     Pre    => Index <= Length (Source),
     Global => null;

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : Positive;
      By     : Character)
   with
     Pre    => Index <= Length (Source),
     Post   => Length (Source) = Length (Source)'Old,
     Global => null;

   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   => Slice'Result'Length = Natural'Max (0, High - Low + 1),
     Global => null;

   function Unbounded_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return Unbounded_String
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   =>
       Length (Unbounded_Slice'Result) = Natural'Max (0, High - Low + 1),
     Global => null;
   pragma Ada_05 (Unbounded_Slice);

   procedure Unbounded_Slice
     (Source : Unbounded_String;
      Target : out Unbounded_String;
      Low    : Positive;
      High   : Natural)
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   => Length (Target) = Natural'Max (0, High - Low + 1),
     Global => null;
   pragma Ada_05 (Unbounded_Slice);

   function "="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function "="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Global => null;

   function "="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function "<"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function "<"
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Global => null;

   function "<"
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function "<="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function "<="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Global => null;

   function "<="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function ">"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function ">"
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Global => null;

   function ">"
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function ">="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   function ">="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Global => null;

   function ">="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Global => null;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre    => Pattern'Length /= 0,
     Global => null;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre    => Pattern'Length /= 0,
     Global => null;

   function Index
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   with
     Global => null;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre    => (if Length (Source) /= 0 then From <= Length (Source))
               and then Pattern'Length /= 0,
     Global => null;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre    => (if Length (Source) /= 0 then From <= Length (Source))
               and then Pattern'Length /= 0,
     Global => null;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   with
     Pre    => (if Length (Source) /= 0 then From <= Length (Source)),
     Global => null;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : Unbounded_String;
      Going  : Direction := Forward) return Natural
   with
     Global => null;

   function Index_Non_Blank
     (Source : Unbounded_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   with
     Pre    => (if Length (Source) /= 0 then From <= Length (Source)),
     Global => null;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre    => Pattern'Length /= 0,
     Global => null;

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre    => Pattern'Length /= 0,
     Global => null;

   function Count
     (Source : Unbounded_String;
      Set    : Maps.Character_Set) return Natural
   with
     Global => null;

   procedure Find_Token
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Pre    => (if Length (Source) /= 0 then From <= Length (Source)),
     Global => null;
   pragma Ada_2012 (Find_Token);

   procedure Find_Token
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Global => null;

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping) return Unbounded_String
   with
     Post   => Length (Translate'Result) = Length (Source),
     Global => null;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping)
   with
     Post   => Length (Source) = Length (Source)'Old,
     Global => null;

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping_Function) return Unbounded_String
   with
     Post   => Length (Translate'Result) = Length (Source),
     Global => null;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping_Function)
   with
     Post   => Length (Source) = Length (Source)'Old,
     Global => null;

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String) return Unbounded_String
   with
     Pre            =>
       Low - 1 <= Length (Source)
         and then (if High >= Low
                   then Low - 1
                     <= Natural'Last - By'Length
                      - Natural'Max (Length (Source) - High, 0)
                   else Length (Source) <= Natural'Last - By'Length),
     Contract_Cases =>
       (High >= Low =>
          Length (Replace_Slice'Result)
        = Low - 1 + By'Length + Natural'Max (Length (Source)'Old - High, 0),
        others      =>
          Length (Replace_Slice'Result) = Length (Source)'Old + By'Length),
     Global         => null;

   procedure Replace_Slice
     (Source : in out Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String)
   with
     Pre            =>
       Low - 1 <= Length (Source)
         and then (if High >= Low
                   then Low - 1
                     <= Natural'Last - By'Length
                      - Natural'Max (Length (Source) - High, 0)
                   else Length (Source) <= Natural'Last - By'Length),
     Contract_Cases =>
       (High >= Low =>
          Length (Source)
        = Low - 1 + By'Length + Natural'Max (Length (Source)'Old - High, 0),
        others      =>
          Length (Source) = Length (Source)'Old + By'Length),
     Global         => null;

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String) return Unbounded_String
   with
     Pre    => Before - 1 <= Length (Source)
                 and then New_Item'Length <= Natural'Last - Length (Source),
     Post   => Length (Insert'Result) = Length (Source) + New_Item'Length,
     Global => null;

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : Positive;
      New_Item : String)
   with
     Pre    => Before - 1 <= Length (Source)
                 and then New_Item'Length <= Natural'Last - Length (Source),
     Post   => Length (Source) = Length (Source)'Old + New_Item'Length,
     Global => null;

   function Overwrite
     (Source   : Unbounded_String;
      Position : Positive;
      New_Item : String) return Unbounded_String
   with
     Pre    => Position - 1 <= Length (Source)
                 and then (if New_Item'Length /= 0
                           then
                             New_Item'Length <= Natural'Last - (Position - 1)),
     Post   =>
       Length (Overwrite'Result)
     = Natural'Max (Length (Source), Position - 1 + New_Item'Length),
     Global => null;

   procedure Overwrite
     (Source   : in out Unbounded_String;
      Position : Positive;
      New_Item : String)
   with
     Pre    => Position - 1 <= Length (Source)
                 and then (if New_Item'Length /= 0
                           then
                             New_Item'Length <= Natural'Last - (Position - 1)),
     Post   =>
       Length (Source)
     = Natural'Max (Length (Source)'Old, Position - 1 + New_Item'Length),

     Global => null;

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural) return Unbounded_String
   with
     Pre            => (if Through <= From then From - 1 <= Length (Source)),
     Contract_Cases =>
       (Through >= From =>
          Length (Delete'Result) = Length (Source) - (Through - From + 1),
        others          =>
          Length (Delete'Result) = Length (Source)),
     Global         => null;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : Positive;
      Through : Natural)
   with
     Pre            => (if Through <= From then From - 1 <= Length (Source)),
     Contract_Cases =>
       (Through >= From =>
          Length (Source) = Length (Source)'Old - (Through - From + 1),
        others          =>
          Length (Source) = Length (Source)'Old),
     Global         => null;

   function Trim
     (Source : Unbounded_String;
      Side   : Trim_End) return Unbounded_String
   with
     Post   => Length (Trim'Result) <= Length (Source),
     Global => null;

   procedure Trim
     (Source : in out Unbounded_String;
      Side   : Trim_End)
   with
     Post   => Length (Source) <= Length (Source)'Old,
     Global => null;

   function Trim
     (Source : Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Unbounded_String
   with
     Post   => Length (Trim'Result) <= Length (Source),
     Global => null;

   procedure Trim
     (Source : in out Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set)
   with
     Post   => Length (Source) <= Length (Source)'Old,
     Global => null;

   function Head
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   with
     Post   => Length (Head'Result) = Count,
     Global => null;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   with
     Post   => Length (Source) = Count,
     Global => null;

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   with
     Post   => Length (Tail'Result) = Count,
     Global => null;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   with
     Post   => Length (Source) = Count,
     Global => null;

   function "*"
     (Left  : Natural;
      Right : Character) return Unbounded_String
   with
     Pre    => Left <= Natural'Last,
     Post   => Length ("*"'Result) = Left,
     Global => null;

   function "*"
     (Left  : Natural;
      Right : String) return Unbounded_String
   with
     Pre    => (if Left /= 0 then Right'Length <= Natural'Last / Left),
     Post   => Length ("*"'Result) = Left * Right'Length,
     Global => null;

   function "*"
     (Left  : Natural;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => (if Left /= 0 then Length (Right) <= Natural'Last / Left),
     Post   => Length ("*"'Result) = Left * Length (Right),
     Global => null;

private
   pragma Inline (Length);

   package AF renames Ada.Finalization;

   type Shared_String (Max_Length : Natural) is limited record
      Counter : System.Atomic_Counters.Atomic_Counter;
      --  Reference counter

      Last : Natural := 0;
      Data : String (1 .. Max_Length);
      --  Last is the index of last significant element of the Data. All
      --  elements with larger indexes are currently insignificant.
   end record;

   type Shared_String_Access is access all Shared_String;

   procedure Reference (Item : not null Shared_String_Access);
   --  Increment reference counter.
   --  Do nothing if Item points to Empty_Shared_String.

   procedure Unreference (Item : not null Shared_String_Access);
   --  Decrement reference counter, deallocate Item when counter goes to zero.
   --  Do nothing if Item points to Empty_Shared_String.

   function Can_Be_Reused
     (Item   : not null Shared_String_Access;
      Length : Natural) return Boolean;
   --  Returns True if Shared_String can be reused. There are two criteria when
   --  Shared_String can be reused: its reference counter must be one (thus
   --  Shared_String is owned exclusively) and its size is sufficient to
   --  store string with specified length effectively.

   function Allocate
     (Required_Length : Natural;
      Reserved_Length : Natural := 0) return not null Shared_String_Access;
   --  Allocates new Shared_String. Actual maximum length of allocated object
   --  is at least the specified required length. Additional storage is
   --  allocated to allow to store up to the specified reserved length when
   --  possible. Returns reference to Empty_Shared_String when requested length
   --  is zero.

   Empty_Shared_String : aliased Shared_String (0);

   function To_Unbounded (S : String) return Unbounded_String
     renames To_Unbounded_String;
   --  This renames are here only to be used in the pragma Stream_Convert

   type Unbounded_String is new AF.Controlled with record
      Reference : not null Shared_String_Access := Empty_Shared_String'Access;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      V : Unbounded_String);

   pragma Stream_Convert (Unbounded_String, To_Unbounded, To_String);
   --  Provide stream routines without dragging in Ada.Streams

   pragma Finalize_Storage_Only (Unbounded_String);
   --  Finalization is required only for freeing storage

   overriding procedure Initialize (Object : in out Unbounded_String);
   overriding procedure Adjust     (Object : in out Unbounded_String);
   overriding procedure Finalize   (Object : in out Unbounded_String);
   pragma Inline (Initialize, Adjust);

   Null_Unbounded_String : constant Unbounded_String :=
                             (AF.Controlled with
                                Reference => Empty_Shared_String'Access);

end Ada.Strings.Unbounded;
