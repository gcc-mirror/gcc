------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S T R I N G S . F I X E D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

package Ada.Strings.Fixed is
pragma Preelaborate (Fixed);

   --------------------------------------------------------------
   -- Copy Procedure for Strings of Possibly Different Lengths --
   --------------------------------------------------------------

   procedure Move
     (Source  : in  String;
      Target  : out String;
      Drop    : in  Truncation := Error;
      Justify : in  Alignment  := Left;
      Pad     : in  Character  := Space);

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source   : in String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
      return     Natural;

   function Index
     (Source   : in String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural;

   function Index
     (Source : in String;
      Set    : in Maps.Character_Set;
      Test   : in Membership := Inside;
      Going  : in Direction  := Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : in String;
      Going  : in Direction := Forward)
      return   Natural;

   function Count
     (Source   : in String;
      Pattern  : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
      return     Natural;

   function Count
     (Source   : in String;
      Pattern  : in String;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural;

   function Count
     (Source   : in String;
      Set      : in Maps.Character_Set)
      return     Natural;

   procedure Find_Token
     (Source : in String;
      Set    : in Maps.Character_Set;
      Test   : in Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : in String;
      Mapping : in Maps.Character_Mapping)
      return    String;

   procedure Translate
     (Source  : in out String;
      Mapping : in Maps.Character_Mapping);

   function Translate
     (Source  : in String;
      Mapping : in Maps.Character_Mapping_Function)
      return    String;

   procedure Translate
     (Source  : in out String;
      Mapping : in Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : in String;
      Low    : in Positive;
      High   : in Natural;
      By     : in String)
      return   String;

   procedure Replace_Slice
     (Source  : in out String;
      Low     : in Positive;
      High    : in Natural;
      By      : in String;
      Drop    : in Truncation := Error;
      Justify : in Alignment  := Left;
      Pad     : in Character  := Space);

   function Insert
     (Source   : in String;
      Before   : in Positive;
      New_Item : in String)
      return     String;

   procedure Insert
     (Source   : in out String;
      Before   : in Positive;
      New_Item : in String;
      Drop     : in Truncation := Error);

   function Overwrite
     (Source   : in String;
      Position : in Positive;
      New_Item : in String)
      return     String;

   procedure Overwrite
     (Source   : in out String;
      Position : in Positive;
      New_Item : in String;
      Drop     : in Truncation := Right);

   function Delete
     (Source  : in String;
      From    : in Positive;
      Through : in Natural)
      return    String;

   procedure Delete
     (Source  : in out String;
      From    : in Positive;
      Through : in Natural;
      Justify : in Alignment := Left;
      Pad     : in Character := Space);

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Trim
     (Source : in String;
      Side   : in Trim_End)
      return   String;

   procedure Trim
     (Source  : in out String;
      Side    : in Trim_End;
      Justify : in Alignment := Left;
      Pad     : in Character := Space);

   function Trim
     (Source : in String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set)
      return   String;

   procedure Trim
     (Source  : in out String;
      Left    : in Maps.Character_Set;
      Right   : in Maps.Character_Set;
      Justify : in Alignment := Strings.Left;
      Pad     : in Character := Space);

   function Head
     (Source : in String;
      Count  : in Natural;
      Pad    : in Character := Space)
      return   String;

   procedure Head
     (Source  : in out String;
      Count   : in Natural;
      Justify : in Alignment := Left;
      Pad     : in Character := Space);

   function Tail
     (Source : in String;
      Count  : in Natural;
      Pad    : in Character := Space)
      return   String;

   procedure Tail
     (Source  : in out String;
      Count   : in Natural;
      Justify : in Alignment := Left;
      Pad     : in Character := Space);

   ----------------------------------
   -- String Constructor Functions --
   ----------------------------------

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  String;

   function "*"
     (Left  : in Natural;
      Right : in String)
      return  String;

end Ada.Strings.Fixed;
