------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . S T R I N G S . U N B O U N D E D                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with Ada.Strings.Maps;
with Ada.Finalization;

package Ada.Strings.Unbounded is
   pragma Preelaborate;

   type Unbounded_String is private;
   pragma Preelaborable_Initialization (Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String;

   function Length (Source : Unbounded_String) return Natural;

   type String_Access is access all String;

   procedure Free (X : in out String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_String
     (Source : String)  return Unbounded_String;

   function To_Unbounded_String
     (Length : Natural) return Unbounded_String;

   function To_String (Source : Unbounded_String) return String;

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String);
   pragma Ada_05 (Set_Unbounded_String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Unbounded_String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Character);

   function "&"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Unbounded_String;

   function "&"
     (Left  : Unbounded_String;
      Right : String) return Unbounded_String;

   function "&"
     (Left  : String;
      Right : Unbounded_String) return Unbounded_String;

   function "&"
     (Left  : Unbounded_String;
      Right : Character) return Unbounded_String;

   function "&"
     (Left  : Character;
      Right : Unbounded_String) return Unbounded_String;

   function Element
     (Source : Unbounded_String;
      Index  : Positive) return Character;

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : Positive;
      By     : Character);

   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String;

   function Unbounded_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return Unbounded_String;
   pragma Ada_05 (Unbounded_Slice);

   procedure Unbounded_Slice
     (Source : Unbounded_String;
      Target : out Unbounded_String;
      Low    : Positive;
      High   : Natural);
   pragma Ada_05 (Unbounded_Slice);

   function "="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean;

   function "="
     (Left  : Unbounded_String;
      Right : String) return Boolean;

   function "="
     (Left  : String;
      Right : Unbounded_String) return Boolean;

   function "<"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean;

   function "<"
     (Left  : Unbounded_String;
      Right : String) return Boolean;

   function "<"
     (Left  : String;
      Right : Unbounded_String) return Boolean;

   function "<="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean;

   function "<="
     (Left  : Unbounded_String;
      Right : String) return Boolean;

   function "<="
     (Left  : String;
      Right : Unbounded_String) return Boolean;

   function ">"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean;

   function ">"
     (Left  : Unbounded_String;
      Right : String) return Boolean;

   function ">"
     (Left  : String;
      Right : Unbounded_String) return Boolean;

   function ">="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean;

   function ">="
     (Left  : Unbounded_String;
      Right : String) return Boolean;

   function ">="
     (Left  : String;
      Right : Unbounded_String) return Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Index
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : Unbounded_String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Unbounded_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Count
     (Source : Unbounded_String;
      Set    : Maps.Character_Set) return Natural;

   procedure Find_Token
     (Source : Unbounded_String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping) return Unbounded_String;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping);

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping_Function) return Unbounded_String;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String) return Unbounded_String;

   procedure Replace_Slice
     (Source : in out Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String);

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String) return Unbounded_String;

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : Positive;
      New_Item : String);

   function Overwrite
     (Source   : Unbounded_String;
      Position : Positive;
      New_Item : String) return Unbounded_String;

   procedure Overwrite
     (Source   : in out Unbounded_String;
      Position : Positive;
      New_Item : String);

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural) return Unbounded_String;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : Positive;
      Through : Natural);

   function Trim
     (Source : Unbounded_String;
      Side   : Trim_End) return Unbounded_String;

   procedure Trim
     (Source : in out Unbounded_String;
      Side   : Trim_End);

   function Trim
     (Source : Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Unbounded_String;

   procedure Trim
     (Source : in out Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set);

   function Head
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space);

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space);

   function "*"
     (Left  : Natural;
      Right : Character) return Unbounded_String;

   function "*"
     (Left  : Natural;
      Right : String) return Unbounded_String;

   function "*"
     (Left  : Natural;
      Right : Unbounded_String) return Unbounded_String;

private
   pragma Inline (Length);

   package AF renames Ada.Finalization;

   Null_String : aliased String := "";

   function To_Unbounded (S : String) return Unbounded_String
     renames To_Unbounded_String;

   type Unbounded_String is new AF.Controlled with record
      Reference : String_Access := Null_String'Access;
      Last      : Natural       := 0;
   end record;
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
                                Last => 0);
end Ada.Strings.Unbounded;
