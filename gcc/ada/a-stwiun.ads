------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . S T R I N G S . W I D E _ U N B O U N D E D            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

with Ada.Strings.Wide_Maps;
with Ada.Finalization;

package Ada.Strings.Wide_Unbounded is
   pragma Preelaborate;

   type Unbounded_Wide_String is private;
   pragma Preelaborable_Initialization (Unbounded_Wide_String);

   Null_Unbounded_Wide_String : constant Unbounded_Wide_String;

   function Length (Source : Unbounded_Wide_String) return Natural;

   type Wide_String_Access is access all Wide_String;

   procedure Free (X : in out Wide_String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_Wide_String
     (Source : Wide_String) return Unbounded_Wide_String;

   function To_Unbounded_Wide_String
     (Length : Natural) return Unbounded_Wide_String;

   function To_Wide_String
     (Source : Unbounded_Wide_String)
      return Wide_String;

   procedure Set_Unbounded_Wide_String
     (Target : out Unbounded_Wide_String;
      Source : Wide_String);
   pragma Ada_05 (Set_Unbounded_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : Unbounded_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : Wide_Character);

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String;

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Unbounded_Wide_String;

   function "&"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String;

   function "&"
     (Left  : Unbounded_Wide_String;
      Right : Wide_Character) return Unbounded_Wide_String;

   function "&"
     (Left  : Wide_Character;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String;

   function Element
     (Source : Unbounded_Wide_String;
      Index  : Positive) return Wide_Character;

   procedure Replace_Element
     (Source : in out Unbounded_Wide_String;
      Index  : Positive;
      By     : Wide_Character);

   function Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_String;

   function Unbounded_Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_String;
   pragma Ada_05 (Unbounded_Slice);

   procedure Unbounded_Slice
     (Source : Unbounded_Wide_String;
      Target : out Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural);
   pragma Ada_05 (Unbounded_Slice);

   function "="
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function "="
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean;

   function "="
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function "<"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function "<"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean;

   function "<"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function "<="
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function "<="
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean;

   function "<="
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function ">"
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function ">"
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean;

   function ">"
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function ">="
     (Left  : Unbounded_Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   function ">="
     (Left  : Unbounded_Wide_String;
      Right : Wide_String) return Boolean;

   function ">="
     (Left  : Wide_String;
      Right : Unbounded_Wide_String) return Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Index
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_Wide_String;
      Set     : Wide_Maps.Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Unbounded_Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Count
     (Source  : Unbounded_Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Count
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set) return Natural;

   procedure Find_Token
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);
   pragma Ada_2012 (Find_Token);

   procedure Find_Token
     (Source : Unbounded_Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping)
      return Unbounded_Wide_String;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping);

   function Translate
     (Source  : Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function)
      return Unbounded_Wide_String;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String) return Unbounded_Wide_String;

   procedure Replace_Slice
     (Source : in out Unbounded_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String);

   function Insert
     (Source   : Unbounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String) return Unbounded_Wide_String;

   procedure Insert
     (Source   : in out Unbounded_Wide_String;
      Before   : Positive;
      New_Item : Wide_String);

   function Overwrite
     (Source   : Unbounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String) return Unbounded_Wide_String;

   procedure Overwrite
     (Source   : in out Unbounded_Wide_String;
      Position : Positive;
      New_Item : Wide_String);

   function Delete
     (Source  : Unbounded_Wide_String;
      From    : Positive;
      Through : Natural) return Unbounded_Wide_String;

   procedure Delete
     (Source  : in out Unbounded_Wide_String;
      From    : Positive;
      Through : Natural);

   function Trim
     (Source : Unbounded_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_String;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Side   : Trim_End);

   function Trim
     (Source : Unbounded_Wide_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set) return Unbounded_Wide_String;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set);

   function Head
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Unbounded_Wide_String;

   procedure Head
     (Source : in out Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space);

   function Tail
     (Source : Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space) return Unbounded_Wide_String;

   procedure Tail
     (Source : in out Unbounded_Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space);

   function "*"
     (Left  : Natural;
      Right : Wide_Character) return Unbounded_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_String) return Unbounded_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_String) return Unbounded_Wide_String;

private
   pragma Inline (Length);

   package AF renames Ada.Finalization;

   Null_Wide_String : aliased Wide_String := "";

   function To_Unbounded_Wide (S : Wide_String) return Unbounded_Wide_String
     renames To_Unbounded_Wide_String;

   type Unbounded_Wide_String is new AF.Controlled with record
      Reference : Wide_String_Access := Null_Wide_String'Access;
      Last      : Natural            := 0;
   end record;

   --  The Unbounded_Wide_String is using a buffered implementation to increase
   --  speed of the Append/Delete/Insert procedures. The Reference string
   --  pointer above contains the current string value and extra room at the
   --  end to be used by the next Append routine. Last is the index of the
   --  string ending character. So the current string value is really
   --  Reference (1 .. Last).

   pragma Stream_Convert
     (Unbounded_Wide_String, To_Unbounded_Wide, To_Wide_String);

   pragma Finalize_Storage_Only (Unbounded_Wide_String);
   --  Finalization is required only for freeing storage

   procedure Initialize (Object : in out Unbounded_Wide_String);
   procedure Adjust     (Object : in out Unbounded_Wide_String);
   procedure Finalize   (Object : in out Unbounded_Wide_String);

   procedure Realloc_For_Chunk
     (Source     : in out Unbounded_Wide_String;
      Chunk_Size : Natural);
   --  Adjust the size allocated for the string. Add at least Chunk_Size so it
   --  is safe to add a string of this size at the end of the current content.
   --  The real size allocated for the string is Chunk_Size + x of the current
   --  string size. This buffered handling makes the Append unbounded string
   --  routines very fast.

   Null_Unbounded_Wide_String : constant Unbounded_Wide_String :=
                                  (AF.Controlled with
                                     Reference => Null_Wide_String'Access,
                                     Last => 0);
end Ada.Strings.Wide_Unbounded;
