------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . S T R I N G S . S E A R C H                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

--  This package contains the search functions from Ada.Strings.Fixed. They
--  are separated out because they are shared by Ada.Strings.Bounded and
--  Ada.Strings.Unbounded, and we don't want to drag other irrelevant stuff
--  from Ada.Strings.Fixed when using the other two packages. We make this
--  a private package, since user programs should access these subprograms
--  via one of the standard string packages.

with Ada.Strings.Maps;

private package Ada.Strings.Search is
   pragma Preelaborate;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

end Ada.Strings.Search;
