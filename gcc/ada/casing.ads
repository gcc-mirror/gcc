------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C A S I N G                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

with Types; use Types;

package Casing is

   --  This package contains data and subprograms to support the feature that
   --  recognizes the letter case styles used in the source program being
   --  compiled, and uses this information for error message formatting, and
   --  for recognizing reserved words that are misused as identifiers.

   -------------------------------
   -- Case Control Declarations --
   -------------------------------

   --  Declaration of type for describing casing convention

   type Casing_Type is (

      All_Upper_Case,
      --  All letters are upper case

      All_Lower_Case,
      --  All letters are lower case

      Mixed_Case,
      --  The initial letter, and any letters after underlines are upper case.
      --  All other letters are lower case

      Unknown
      --  Used if an identifier does not distinguish between the above cases,
      --  (e.g. X, Y_3, M4, A_B, or if it is inconsistent ABC_def).
   );

   ------------------------------
   -- Case Control Subprograms --
   ------------------------------

   procedure Set_Casing (C : Casing_Type; D : Casing_Type := Mixed_Case);
   --  Takes the name stored in the first Name_Len positions of Name_Buffer
   --  and modifies it to be consistent with the casing given by C, or if
   --  C = Unknown, then with the casing given by D. The name is basically
   --  treated as an identifier, except that special separator characters
   --  other than underline are permitted and treated like underlines (this
   --  handles cases like minus and period in unit names, apostrophes in error
   --  messages, angle brackets in names like <any_type>, etc).

   procedure Set_All_Upper_Case;
   pragma Inline (Set_All_Upper_Case);
   --  This procedure is called with an identifier name stored in Name_Buffer.
   --  On return, the identifier is converted to all upper case. The call is
   --  equivalent to Set_Casing (All_Upper_Case).

   function Determine_Casing (Ident : Text_Buffer) return Casing_Type;
   --  Determines the casing of the identifier/keyword string Ident

end Casing;
