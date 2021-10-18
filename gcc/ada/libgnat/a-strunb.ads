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

with Ada.Strings.Maps;
with Ada.Finalization;
private with Ada.Strings.Text_Buffers;

--  The language-defined package Strings.Unbounded provides a private type
--  Unbounded_String and a set of operations. An object of type
--  Unbounded_String represents a String whose low bound is 1 and whose length
--  can vary conceptually between 0 and Natural'Last. The subprograms for
--  fixed-length string handling are either overloaded directly for
--  Unbounded_String, or are modified as needed to reflect the flexibility in
--  length. Since the Unbounded_String type is private, relevant constructor
--  and selector operations are provided.

package Ada.Strings.Unbounded with
  SPARK_Mode,
  Initial_Condition => Length (Null_Unbounded_String) = 0
is
   pragma Preelaborate;

   type Unbounded_String is private with
     Default_Initial_Condition => Length (Unbounded_String) = 0;
   pragma Preelaborable_Initialization (Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String;
   --  Represents the null String. If an object of type Unbounded_String is not
   --  otherwise initialized, it will be initialized to the same value as
   --  Null_Unbounded_String.

   function Length (Source : Unbounded_String) return Natural with
     Global => null;
   --  Returns the length of the String represented by Source

   type String_Access is access all String;
   --  Provides a (nonprivate) access type for explicit processing of
   --  unbounded-length strings.

   procedure Free (X : in out String_Access) with SPARK_Mode => Off;
   --  Performs an unchecked deallocation of an object of type String_Access

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_String
     (Source : String)  return Unbounded_String
   with
     Post   => Length (To_Unbounded_String'Result) = Source'Length,
     Global => null;
   --  Returns an Unbounded_String that represents Source

   function To_Unbounded_String
     (Length : Natural) return Unbounded_String
   with
     Post   =>
       Ada.Strings.Unbounded.Length (To_Unbounded_String'Result) = Length,
     Global => null;
   --  Returns an Unbounded_String that represents an uninitialized String
   --  whose length is Length.

   function To_String (Source : Unbounded_String) return String with
     Post   => To_String'Result'Length = Length (Source),
     Global => null;
   --  Returns the String with lower bound 1 represented by Source

   --  To_String and To_Unbounded_String are related as follows:
   --
   --  * If S is a String, then To_String (To_Unbounded_String (S)) = S.
   --
   --  * If U is an Unbounded_String, then
   --    To_Unbounded_String (To_String (U)) = U.

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String)
   with
     Global => null;
   pragma Ada_05 (Set_Unbounded_String);
   --  Sets Target to an Unbounded_String that represents Source

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

   --  For each of the Append procedures, the resulting string represented by
   --  the Source parameter is given by the concatenation of the original value
   --  of Source and the value of New_Item.

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

   --  Each of the "&" functions returns an Unbounded_String obtained by
   --  concatenating the string or character given or represented by one of the
   --  parameters, with the string or character given or represented by the
   --  other parameter, and applying To_Unbounded_String to the concatenation
   --  result string.

   function Element
     (Source : Unbounded_String;
      Index  : Positive) return Character
   with
     Pre    => Index <= Length (Source),
     Global => null;
   --  Returns the character at position Index in the string represented by
   --  Source; propagates Index_Error if Index > Length (Source).

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : Positive;
      By     : Character)
   with
     Pre    => Index <= Length (Source),
     Post   => Length (Source) = Length (Source)'Old,
     Global => null;
   --  Updates Source such that the character at position Index in the string
   --  represented by Source is By; propagates Index_Error if
   --  Index > Length (Source).

   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   => Slice'Result'Length = Natural'Max (0, High - Low + 1),
     Global => null;
   --  Returns the slice at positions Low through High in the string
   --  represented by Source; propagates Index_Error if
   --  Low > Length (Source) + 1 or High > Length (Source). The bounds of the
   --  returned string are Low and High.

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
   --  Returns the slice at positions Low through High in the string
   --  represented by Source as an Unbounded_String. This propagates
   --  Index_Error if Low > Length(Source) + 1 or High > Length (Source).

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
   --  Sets Target to the Unbounded_String representing the slice at positions
   --  Low through High in the string represented by Source. This propagates
   --  Index_Error if Low > Length(Source) + 1 or High > Length (Source).

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

   --  Each of the functions "=", "<", ">", "<=", and ">=" returns the same
   --  result as the corresponding String operation applied to the String
   --  values given or represented by Left and Right.

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

   --  Each of the search subprograms (Index, Index_Non_Blank, Count,
   --  Find_Token) has the same effect as the corresponding subprogram in
   --  Strings.Fixed applied to the string represented by the Unbounded_String
   --  parameter.

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

   --  The Translate function has an analogous effect to the corresponding
   --  subprogram in Strings.Fixed. The translation is applied to the string
   --  represented by the Unbounded_String parameter, and the result is
   --  converted (via To_Unbounded_String) to an Unbounded_String.

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

   --  Each of the transformation functions (Replace_Slice, Insert, Overwrite,
   --  Delete), selector functions (Trim, Head, Tail), and constructor
   --  functions ("*") is likewise analogous to its corresponding subprogram in
   --  Strings.Fixed. For each of the subprograms, the corresponding
   --  fixed-length string subprogram is applied to the string represented by
   --  the Unbounded_String parameter, and To_Unbounded_String is applied the
   --  result string.
   --
   --  For each of the procedures Translate, Replace_Slice, Insert, Overwrite,
   --  Delete, Trim, Head, and Tail, the resulting string represented by the
   --  Source parameter is given by the corresponding function for fixed-length
   --  strings applied to the string represented by Source's original value.

private
   pragma SPARK_Mode (Off);  --  Controlled types are not in SPARK

   pragma Inline (Length);

   package AF renames Ada.Finalization;

   Null_String : aliased String := "";

   function To_Unbounded (S : String) return Unbounded_String
     renames To_Unbounded_String;

   type Unbounded_String is new AF.Controlled with record
      Reference : String_Access := Null_String'Access;
      Last      : Natural       := 0;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      V : Unbounded_String);

   --  The Unbounded_String is using a buffered implementation to increase
   --  speed of the Append/Delete/Insert procedures. The Reference string
   --  pointer above contains the current string value and extra room at the
   --  end to be used by the next Append routine. Last is the index of the
   --  string ending character. So the current string value is really
   --  Reference (1 .. Last).

   pragma Stream_Convert (Unbounded_String, To_Unbounded, To_String);
   --  Provide stream routines without dragging in Ada.Streams

   pragma Finalize_Storage_Only (Unbounded_String);
   --  Finalization is required only for freeing storage

   procedure Initialize (Object : in out Unbounded_String);
   procedure Adjust     (Object : in out Unbounded_String);
   procedure Finalize   (Object : in out Unbounded_String);

   procedure Realloc_For_Chunk
     (Source     : in out Unbounded_String;
      Chunk_Size : Natural);
   pragma Inline (Realloc_For_Chunk);
   --  Adjust the size allocated for the string. Add at least Chunk_Size so it
   --  is safe to add a string of this size at the end of the current content.
   --  The real size allocated for the string is Chunk_Size + x of the current
   --  string size. This buffered handling makes the Append unbounded string
   --  routines very fast. This spec is in the private part so that it can be
   --  accessed from children (e.g. from Unbounded.Text_IO).

   Null_Unbounded_String : constant Unbounded_String :=
                             (AF.Controlled with
                                Reference => Null_String'Access,
                                Last      => 0);
end Ada.Strings.Unbounded;
