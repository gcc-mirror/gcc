------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . S T R I N G S . W I D E _ U N B O U N D E D            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

with Ada.Strings.Wide_Maps;
with Ada.Finalization;

package Ada.Strings.Wide_Unbounded is
pragma Preelaborate (Wide_Unbounded);

   type Unbounded_Wide_String is private;

   Null_Unbounded_Wide_String : constant Unbounded_Wide_String;

   function Length (Source : Unbounded_Wide_String) return Natural;

   type Wide_String_Access is access all Wide_String;

   procedure Free (X : in out Wide_String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_Wide_String
     (Source : Wide_String)
      return   Unbounded_Wide_String;

   function To_Unbounded_Wide_String
     (Length : in Natural)
      return   Unbounded_Wide_String;

   function To_Wide_String
     (Source : Unbounded_Wide_String)
      return   Wide_String;

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : in Unbounded_Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : in Wide_String);

   procedure Append
     (Source   : in out Unbounded_Wide_String;
      New_Item : in Wide_Character);

   function "&"
     (Left, Right : Unbounded_Wide_String)
      return        Unbounded_Wide_String;

   function "&"
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Unbounded_Wide_String;

   function "&"
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Unbounded_Wide_String;

   function "&"
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_Character)
      return  Unbounded_Wide_String;

   function "&"
     (Left  : in Wide_Character;
      Right : in Unbounded_Wide_String)
      return  Unbounded_Wide_String;

   function Element
     (Source : in Unbounded_Wide_String;
      Index  : in Positive)
      return   Wide_Character;

   procedure Replace_Element
     (Source : in out Unbounded_Wide_String;
      Index  : in Positive;
      By     : Wide_Character);

   function Slice
     (Source : in Unbounded_Wide_String;
      Low    : in Positive;
      High   : in Natural)
      return   Wide_String;

   function "="
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function "="
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean;

   function "="
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function "<"
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function "<"
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean;

   function "<"
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function "<="
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function "<="
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean;

   function "<="
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function ">"
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function ">"
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean;

   function ">"
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function ">="
     (Left  : in Unbounded_Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   function ">="
     (Left  : in Unbounded_Wide_String;
      Right : in Wide_String)
      return  Boolean;

   function ">="
     (Left  : in Wide_String;
      Right : in Unbounded_Wide_String)
      return  Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source   : in Unbounded_Wide_String;
      Pattern  : in Wide_String;
      Going    : in Direction := Forward;
      Mapping  : in Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return     Natural;

   function Index
     (Source   : in Unbounded_Wide_String;
      Pattern  : in Wide_String;
      Going    : in Direction := Forward;
      Mapping  : in Wide_Maps.Wide_Character_Mapping_Function)
      return     Natural;

   function Index
     (Source : in Unbounded_Wide_String;
      Set    : in Wide_Maps.Wide_Character_Set;
      Test   : in Membership := Inside;
      Going  : in Direction  := Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : in Unbounded_Wide_String;
      Going  : in Direction := Forward)
      return   Natural;

   function Count
     (Source  : in Unbounded_Wide_String;
      Pattern : in Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return    Natural;

   function Count
     (Source   : in Unbounded_Wide_String;
      Pattern  : in Wide_String;
      Mapping  : in Wide_Maps.Wide_Character_Mapping_Function)
      return     Natural;

   function Count
     (Source : in Unbounded_Wide_String;
      Set    : in Wide_Maps.Wide_Character_Set)
      return   Natural;

   procedure Find_Token
     (Source : in Unbounded_Wide_String;
      Set    : in Wide_Maps.Wide_Character_Set;
      Test   : in Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- Wide_String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : in Unbounded_Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping)
      return    Unbounded_Wide_String;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping);

   function Translate
     (Source  : in Unbounded_Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
      return    Unbounded_Wide_String;

   procedure Translate
     (Source  : in out Unbounded_Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function);

   ---------------------------------------
   -- Wide_String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : in Unbounded_Wide_String;
      Low    : in Positive;
      High   : in Natural;
      By     : in Wide_String)
      return   Unbounded_Wide_String;

   procedure Replace_Slice
     (Source   : in out Unbounded_Wide_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Wide_String);

   function Insert
     (Source   : in Unbounded_Wide_String;
      Before   : in Positive;
      New_Item : in Wide_String)
      return     Unbounded_Wide_String;

   procedure Insert
     (Source   : in out Unbounded_Wide_String;
      Before   : in Positive;
      New_Item : in Wide_String);

   function Overwrite
     (Source   : in Unbounded_Wide_String;
      Position : in Positive;
      New_Item : in Wide_String)
      return     Unbounded_Wide_String;

   procedure Overwrite
     (Source    : in out Unbounded_Wide_String;
      Position  : in Positive;
      New_Item  : in Wide_String);

   function Delete
     (Source  : in Unbounded_Wide_String;
      From    : in Positive;
      Through : in Natural)
      return    Unbounded_Wide_String;

   procedure Delete
     (Source  : in out Unbounded_Wide_String;
      From    : in Positive;
      Through : in Natural);

   function Trim
     (Source : in Unbounded_Wide_String;
      Side   : in Trim_End)
      return   Unbounded_Wide_String;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Side   : in Trim_End);

   function Trim
     (Source : in Unbounded_Wide_String;
      Left   : in Wide_Maps.Wide_Character_Set;
      Right  : in Wide_Maps.Wide_Character_Set)
      return   Unbounded_Wide_String;

   procedure Trim
     (Source : in out Unbounded_Wide_String;
      Left   : in Wide_Maps.Wide_Character_Set;
      Right  : in Wide_Maps.Wide_Character_Set);

   function Head
     (Source : in Unbounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space)
      return   Unbounded_Wide_String;

   procedure Head
     (Source : in out Unbounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space);

   function Tail
     (Source : in Unbounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space)
      return   Unbounded_Wide_String;

   procedure Tail
     (Source : in out Unbounded_Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space);

   function "*"
     (Left  : in Natural;
      Right : in Wide_Character)
      return  Unbounded_Wide_String;

   function "*"
     (Left  : in Natural;
      Right : in Wide_String)
      return  Unbounded_Wide_String;

   function "*"
     (Left  : in Natural;
      Right : in Unbounded_Wide_String)
      return  Unbounded_Wide_String;

private
   pragma Inline (Length);

   package AF renames Ada.Finalization;

   Null_Wide_String : aliased Wide_String := "";

   function To_Unbounded_Wide (S : Wide_String) return Unbounded_Wide_String
     renames To_Unbounded_Wide_String;

   type Unbounded_Wide_String is new AF.Controlled with record
      Reference : Wide_String_Access := Null_Wide_String'Access;
   end record;

   pragma Stream_Convert
     (Unbounded_Wide_String, To_Unbounded_Wide, To_Wide_String);

   pragma Finalize_Storage_Only (Unbounded_Wide_String);

   procedure Initialize (Object : in out Unbounded_Wide_String);
   procedure Adjust     (Object : in out Unbounded_Wide_String);
   procedure Finalize   (Object : in out Unbounded_Wide_String);

   Null_Unbounded_Wide_String : constant Unbounded_Wide_String :=
     (AF.Controlled with Reference => Null_Wide_String'Access);

end Ada.Strings.Wide_Unbounded;
