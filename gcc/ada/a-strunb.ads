------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                A D A . S T R I N G S . U N B O U N D E D                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Ada.Strings.Maps;
with Ada.Finalization;

package Ada.Strings.Unbounded is
pragma Preelaborate (Unbounded);

   type Unbounded_String is private;

   Null_Unbounded_String : constant Unbounded_String;

   function Length (Source : Unbounded_String) return Natural;

   type String_Access is access all String;

   procedure Free (X : in out String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_String (Source : String)     return Unbounded_String;
   function To_Unbounded_String (Length : in Natural) return Unbounded_String;

   function To_String (Source : Unbounded_String) return String;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in Unbounded_String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in Character);

   function "&" (Left, Right : Unbounded_String) return Unbounded_String;

   function "&"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Unbounded_String;

   function "&"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Unbounded_String;

   function "&"
     (Left  : in Unbounded_String;
      Right : in Character)
      return  Unbounded_String;

   function "&"
     (Left  : in Character;
      Right : in Unbounded_String)
      return  Unbounded_String;

   function Element
     (Source : in Unbounded_String;
      Index  : in Positive)
      return   Character;

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : in Positive;
      By     : Character);

   function Slice
     (Source : in Unbounded_String;
      Low    : in Positive;
      High   : in Natural)
      return   String;

   function "=" (Left, Right : in Unbounded_String) return Boolean;

   function "="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function "="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function "<" (Left, Right : in Unbounded_String) return Boolean;

   function "<"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function "<"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function "<=" (Left, Right : in Unbounded_String) return Boolean;

   function "<="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function "<="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function ">" (Left, Right : in Unbounded_String) return Boolean;

   function ">"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function ">"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function ">=" (Left, Right : in Unbounded_String) return Boolean;

   function ">="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function ">="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
      return     Natural;

   function Index
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural;

   function Index
     (Source : in Unbounded_String;
      Set    : in Maps.Character_Set;
      Test   : in Membership := Inside;
      Going  : in Direction  := Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : in Unbounded_String;
      Going  : in Direction := Forward)
      return   Natural;

   function Count
     (Source  : in Unbounded_String;
      Pattern : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity)
      return    Natural;

   function Count
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural;

   function Count
     (Source : in Unbounded_String;
      Set    : in Maps.Character_Set)
      return   Natural;

   procedure Find_Token
     (Source : in Unbounded_String;
      Set    : in Maps.Character_Set;
      Test   : in Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : in Unbounded_String;
      Mapping : in Maps.Character_Mapping)
      return    Unbounded_String;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping);

   function Translate
     (Source  : in Unbounded_String;
      Mapping : in Maps.Character_Mapping_Function)
      return    Unbounded_String;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : in Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : in Unbounded_String;
      Low    : in Positive;
      High   : in Natural;
      By     : in String)
      return   Unbounded_String;

   procedure Replace_Slice
     (Source   : in out Unbounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in String);

   function Insert
     (Source   : in Unbounded_String;
      Before   : in Positive;
      New_Item : in String)
      return     Unbounded_String;

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : in Positive;
      New_Item : in String);

   function Overwrite
     (Source   : in Unbounded_String;
      Position : in Positive;
      New_Item : in String)
      return     Unbounded_String;

   procedure Overwrite
     (Source    : in out Unbounded_String;
      Position  : in Positive;
      New_Item  : in String);

   function Delete
     (Source  : in Unbounded_String;
      From    : in Positive;
      Through : in Natural)
      return    Unbounded_String;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : in Positive;
      Through : in Natural);

   function Trim
     (Source : in Unbounded_String;
      Side   : in Trim_End)
      return   Unbounded_String;

   procedure Trim
     (Source : in out Unbounded_String;
      Side   : in Trim_End);

   function Trim
     (Source : in Unbounded_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set)
      return   Unbounded_String;

   procedure Trim
     (Source : in out Unbounded_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set);

   function Head
     (Source : in Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space)
      return   Unbounded_String;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space);

   function Tail
     (Source : in Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space)
      return   Unbounded_String;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space);

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  Unbounded_String;

   function "*"
     (Left  : in Natural;
      Right : in String)
      return  Unbounded_String;

   function "*"
     (Left  : in Natural;
      Right : in Unbounded_String)
      return  Unbounded_String;

private
   pragma Inline (Length);

   package AF renames Ada.Finalization;

   Null_String : aliased String := "";

   function To_Unbounded (S : String) return Unbounded_String
     renames To_Unbounded_String;

   type Unbounded_String is new AF.Controlled with record
      Reference : String_Access := Null_String'Access;
   end record;

   pragma Stream_Convert (Unbounded_String, To_Unbounded, To_String);

   pragma Finalize_Storage_Only (Unbounded_String);

   procedure Initialize (Object : in out Unbounded_String);
   procedure Adjust     (Object : in out Unbounded_String);
   procedure Finalize   (Object : in out Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String :=
     (AF.Controlled with Reference => Null_String'Access);

end Ada.Strings.Unbounded;
