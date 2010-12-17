------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . S T R I N G S . U N B O U N D E D                --
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
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an implementation of Ada.Strings.Unbounded that uses
--  reference counts to implement copy on modification (rather than copy on
--  assignment). This is significantly more efficient on many targets.

--  This version is supported on:
--    - all Alpha platforms
--    - all ia64 platforms
--    - all PowerPC platforms
--    - all SPARC V9 platforms
--    - all x86_64 platforms

   --  This package uses several techniques to increase speed:

   --   - Implicit sharing or copy-on-write. An Unbounded_String contains only
   --     the reference to the data which is shared between several instances.
   --     The shared data is reallocated only when its value is changed and
   --     the object mutation can't be used or it is unefficient to use it.

   --   - Object mutation. Shared data object can be reused without memory
   --     reallocation when all of the following requirements are met:
   --      - the shared data object is no longer used by anyone else;
   --      - the size is sufficient to store the new value;
   --      - the gap after reuse is less then a defined threshold.

   --   - Memory preallocation. Most of used memory allocation algorithms
   --     align allocated segments on the some boundary, thus some amount of
   --     additional memory can be preallocated without any impact. Such
   --     preallocated memory can used later by Append/Insert operations
   --     without reallocation.

   --  Reference counting uses GCC builtin atomic operations, which allows to
   --  safely share internal data between Ada tasks. Nevertheless, this doesn't
   --  make objects of Unbounded_String thread-safe: each instance can't be
   --  accessed by several tasks simulatenously.

with Ada.Strings.Maps;
private with Ada.Finalization;
private with Interfaces;

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
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);
   pragma Ada_2012 (Find_Token);

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

   type Shared_String (Max_Length : Natural) is limited record
      Counter : aliased Interfaces.Unsigned_32 := 1;
      --  Reference counter

      Last : Natural := 0;
      Data : String (1 .. Max_Length);
      --  Last is the index of last significant element of the Data. All
      --  elements with larger indexes are currently insignificant.
   end record;

   type Shared_String_Access is access all Shared_String;

   procedure Reference (Item : not null Shared_String_Access);
   --  Increment reference counter

   procedure Unreference (Item : not null Shared_String_Access);
   --  Decrement reference counter, deallocate Item when counter goes to zero

   function Can_Be_Reused
     (Item   : Shared_String_Access;
      Length : Natural) return Boolean;
   --  Returns True if Shared_String can be reused. There are two criteria when
   --  Shared_String can be reused: its reference counter must be one (thus
   --  Shared_String is owned exclusively) and its size is sufficient to
   --  store string with specified length effectively.

   function Allocate (Max_Length : Natural) return Shared_String_Access;
   --  Allocates new Shared_String with at least specified maximum length.
   --  Actual maximum length of the allocated Shared_String can be sligtly
   --  greater. Returns reference to Empty_Shared_String when requested length
   --  is zero.

   Empty_Shared_String : aliased Shared_String (0);

   function To_Unbounded (S : String) return Unbounded_String
     renames To_Unbounded_String;
   --  This renames are here only to be used in the pragma Stream_Convert

   type Unbounded_String is new AF.Controlled with record
      Reference : Shared_String_Access := Empty_Shared_String'Access;
   end record;

   pragma Stream_Convert (Unbounded_String, To_Unbounded, To_String);
   --  Provide stream routines without dragging in Ada.Streams

   pragma Finalize_Storage_Only (Unbounded_String);
   --  Finalization is required only for freeing storage

   overriding procedure Initialize (Object : in out Unbounded_String);
   overriding procedure Adjust     (Object : in out Unbounded_String);
   overriding procedure Finalize   (Object : in out Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String :=
                             (AF.Controlled with
                                Reference => Empty_Shared_String'Access);

end Ada.Strings.Unbounded;
