------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . S T R I N G S . W I D E _ W I D E _ U N B O U N D E D       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This version is supported on:
--    - all Alpha platforms
--    - all ia64 platforms
--    - all PowerPC platforms
--    - all SPARC V9 platforms
--    - all x86 platforms
--    - all x86_64 platforms

with Ada.Strings.Wide_Wide_Maps;
private with Ada.Finalization;
private with System.Atomic_Counters;

package Ada.Strings.Wide_Wide_Unbounded is
   pragma Preelaborate;

   type Unbounded_Wide_Wide_String is private;
   pragma Preelaborable_Initialization (Unbounded_Wide_Wide_String);

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String;

   function Length (Source : Unbounded_Wide_Wide_String) return Natural;

   type Wide_Wide_String_Access is access all Wide_Wide_String;

   procedure Free (X : in out Wide_Wide_String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_Wide_Wide_String
     (Source : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function To_Unbounded_Wide_Wide_String
     (Length : Natural) return Unbounded_Wide_Wide_String;

   function To_Wide_Wide_String
     (Source : Unbounded_Wide_Wide_String) return Wide_Wide_String;

   procedure Set_Unbounded_Wide_Wide_String
     (Target : out Unbounded_Wide_Wide_String;
      Source : Wide_Wide_String);
   pragma Ada_05 (Set_Unbounded_Wide_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item : Unbounded_Wide_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_Character);

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String;

   function "&"
     (Left  : Wide_Wide_Character;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function Element
     (Source : Unbounded_Wide_Wide_String;
      Index  : Positive) return Wide_Wide_Character;

   procedure Replace_Element
     (Source : in out Unbounded_Wide_Wide_String;
      Index  : Positive;
      By     : Wide_Wide_Character);

   function Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Wide_Wide_String;

   function Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural) return Unbounded_Wide_Wide_String;
   pragma Ada_05 (Unbounded_Slice);

   procedure Unbounded_Slice
     (Source : Unbounded_Wide_Wide_String;
      Target : out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural);
   pragma Ada_05 (Unbounded_Slice);

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function "="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function "<"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function "<="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function "<="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">"
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function ">"
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   function ">="
     (Left  : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_String) return Boolean;

   function ">="
     (Left  : Wide_Wide_String;
      Right : Unbounded_Wide_Wide_String) return Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Index
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_Wide_Wide_String;
      Set     : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : Unbounded_Wide_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Unbounded_Wide_Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;

   function Count
     (Source  : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Count
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural;

   procedure Find_Token
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);
   pragma Ada_2012 (Find_Token);

   procedure Find_Token
     (Source : Unbounded_Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Unbounded_Wide_Wide_String;

   procedure Translate
     (Source  : in out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping);

   function Translate
     (Source  : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Unbounded_Wide_Wide_String;

   procedure Translate
     (Source  : in out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   procedure Replace_Slice
     (Source : in out Unbounded_Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String);

   function Insert
     (Source   : Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   procedure Insert
     (Source   : in out Unbounded_Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String);

   function Overwrite
     (Source   : Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   procedure Overwrite
     (Source   : in out Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String);

   function Delete
     (Source  : Unbounded_Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Unbounded_Wide_Wide_String;

   procedure Delete
     (Source  : in out Unbounded_Wide_Wide_String;
      From    : Positive;
      Through : Natural);

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Side   : Trim_End) return Unbounded_Wide_Wide_String;

   procedure Trim
     (Source : in out Unbounded_Wide_Wide_String;
      Side   : Trim_End);

   function Trim
     (Source : Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Unbounded_Wide_Wide_String;

   procedure Trim
     (Source : in out Unbounded_Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set);

   function Head
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space)
      return Unbounded_Wide_Wide_String;

   procedure Head
     (Source : in out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space);

   function Tail
     (Source : Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space)
      return Unbounded_Wide_Wide_String;

   procedure Tail
     (Source : in out Unbounded_Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Wide_Wide_Space);

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_Character) return Unbounded_Wide_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

private
   pragma Inline (Length);

   package AF renames Ada.Finalization;

   type Shared_Wide_Wide_String (Max_Length : Natural) is limited record
      Counter : System.Atomic_Counters.Atomic_Counter;
      --  Reference counter

      Last : Natural := 0;
      Data : Wide_Wide_String (1 .. Max_Length);
      --  Last is the index of last significant element of the Data. All
      --  elements with larger indexes are just extra room for expansion.
   end record;

   type Shared_Wide_Wide_String_Access is access all Shared_Wide_Wide_String;

   procedure Reference (Item : not null Shared_Wide_Wide_String_Access);
   --  Increment reference counter.

   procedure Unreference (Item : not null Shared_Wide_Wide_String_Access);
   --  Decrement reference counter. Deallocate Item when reference counter is
   --  zero.

   function Can_Be_Reused
     (Item   : Shared_Wide_Wide_String_Access;
      Length : Natural) return Boolean;
   --  Returns True if Shared_Wide_Wide_String can be reused. There are two
   --  criteria when Shared_Wide_Wide_String can be reused: its reference
   --  counter must be one (thus Shared_Wide_Wide_String is owned exclusively)
   --  and its size is sufficient to store string with specified length
   --  effectively.

   function Allocate
     (Max_Length : Natural) return Shared_Wide_Wide_String_Access;
   --  Allocates new Shared_Wide_Wide_String with at least specified maximum
   --  length. Actual maximum length of the allocated Shared_Wide_Wide_String
   --  can be slightly greater. Returns reference to
   --  Empty_Shared_Wide_Wide_String when requested length is zero.

   Empty_Shared_Wide_Wide_String : aliased Shared_Wide_Wide_String (0);

   function To_Unbounded
     (S : Wide_Wide_String) return Unbounded_Wide_Wide_String
     renames To_Unbounded_Wide_Wide_String;
   --  This renames are here only to be used in the pragma Stream_Convert.

   type Unbounded_Wide_Wide_String is new AF.Controlled with record
      Reference : Shared_Wide_Wide_String_Access :=
                    Empty_Shared_Wide_Wide_String'Access;
   end record;

   --  The Unbounded_Wide_Wide_String uses several techniques to increase speed
   --  of the application:

   --   - implicit sharing or copy-on-write. Unbounded_Wide_Wide_String
   --     contains only the reference to the data which is shared between
   --     several instances. The shared data is reallocated only when its value
   --     is changed and the object mutation can't be used or it is inefficient
   --     to use it;

   --   - object mutation. Shared data object can be reused without memory
   --     reallocation when all of the following requirements are meat:
   --      - shared data object don't used anywhere longer;
   --      - its size is sufficient to store new value;
   --      - the gap after reuse is less than some threshold.

   --   - memory preallocation. Most of used memory allocation algorithms
   --     aligns allocated segment on the some boundary, thus some amount of
   --     additional memory can be preallocated without any impact. Such
   --     preallocated memory can used later by Append/Insert operations
   --     without reallocation.

   --  Reference counting uses GCC builtin atomic operations, which allows safe
   --  sharing of internal data between Ada tasks. Nevertheless, this does not
   --  make objects of Unbounded_String thread-safe: an instance cannot be
   --  accessed by several tasks simultaneously.

   pragma Stream_Convert
     (Unbounded_Wide_Wide_String, To_Unbounded, To_Wide_Wide_String);
   --  Provide stream routines without dragging in Ada.Streams

   pragma Finalize_Storage_Only (Unbounded_Wide_Wide_String);
   --  Finalization is required only for freeing storage

   overriding procedure Initialize
     (Object : in out Unbounded_Wide_Wide_String);
   overriding procedure Adjust
     (Object : in out Unbounded_Wide_Wide_String);
   overriding procedure Finalize
     (Object : in out Unbounded_Wide_Wide_String);
   pragma Inline (Initialize, Adjust);

   Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String :=
                                       (AF.Controlled with
                                          Reference =>
                                            Empty_Shared_Wide_Wide_String'
                                              Access);

end Ada.Strings.Wide_Wide_Unbounded;
