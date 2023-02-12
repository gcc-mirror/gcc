------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . S T R I N G S . W I D E _ W I D E _ S E A R C H          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This package contains search functions from Ada.Strings.Wide_Wide_Fixed.
--  They are separated because Ada.Strings.Wide_Wide_Bounded shares these
--  search functions with Ada.Strings.Wide_Wide_Unbounded, and we don't want to
--  drag in other irrelevant stuff from Ada.Strings.Wide_Wide_Fixed when using
--  the other two packages. We make this a private package, since user programs
--  should access these subprograms via one of the standard string packages.

with Ada.Strings.Wide_Wide_Maps;

private package Ada.Strings.Wide_Wide_Search is
   pragma Preelaborate;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Index
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Set     : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Wide_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Wide_Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Count
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural;

   procedure Find_Token
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   procedure Find_Token
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

end Ada.Strings.Wide_Wide_Search;
