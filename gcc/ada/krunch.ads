------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               K R U N C H                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This procedure implements file name crunching

--    First, the name is divided into segments separated by minus signs and
--    underscores, then all minus signs and underscores are eliminated. If
--    this leaves the name short enough, we are done.

--    If not, then the longest segment is located (left-most if there are
--    two of equal length), and shortened by dropping its last character.
--    This is repeated until the name is short enough.

--    As an example, consider the krunch of our-strings-wide_fixed.adb
--    to fit the name into 8 characters as required by DOS:

--      our-strings-wide_fixed      22
--      our strings wide fixed      19
--      our string  wide fixed      18
--      our strin   wide fixed      17
--      our stri    wide fixed      16
--      our stri    wide fixe       15
--      our str     wide fixe       14
--      our str     wid  fixe       13
--      our str     wid  fix        12
--      ou  str     wid  fix        11
--      ou  st      wid  fix        10
--      ou  st      wi   fix         9
--      ou  st      wi   fi          8

--      Final file name: OUSTWIFX.ADB

--    A special rule applies for children of System, Ada, Gnat, and Interfaces.
--    In these cases, the following special prefix replacements occur:

--       ada-        replaced by  a-
--       gnat-       replaced by  g-
--       interfaces- replaced by  i-
--       system-     replaced by  s-

--    The rest of the name is krunched in the usual manner described above.
--    In addition, these names, as well as the names of the renamed packages
--    from the obsolescent features annex, are always krunched to 8 characters
--    regardless of the setting of Maxlen.

--    As an example of this special rule, consider ada-strings-wide_fixed.adb
--    which gets krunched as follows:

--      ada-strings-wide_fixed      22
--      a-  strings wide fixed      18
--      a-  string  wide fixed      17
--      a-  strin   wide fixed      16
--      a-  stri    wide fixed      15
--      a-  stri    wide fixe       14
--      a-  str     wide fixe       13
--      a-  str     wid  fixe       12
--      a-  str     wid  fix        11
--      a-  st      wid  fix        10
--      a-  st      wi   fix         9
--      a-  st      wi   fi          8

--      Final file name: A-STWIFX.ADB

--  Since children of units named A, G, I or S might conflict with the names
--  of predefined units, the naming rule in that case is that the first hyphen
--  is replaced by a tilde sign.

--  Note: as described below, this special treatment of predefined library
--  unit file names can be inhibited by setting the No_Predef flag.

--  Of course there is no guarantee that this algorithm results in uniquely
--  crunched names (nor, obviously, is there any algorithm which would do so)
--  In fact we run into such a case in the standard library routines with
--  children of Wide_Text_IO, so a special rule is applied to deal with this
--  clash, namely the prefix ada-wide_text_io- is replaced by a-wt- and then
--  the normal crunching rules are applied, so that for example, the unit:

--    Ada.Wide_Text_IO.Float_IO

--  has the file name

--    a-wtflio

--  More problems arise with Wide_Wide, so we replace this sequence by
--  a z (which is not used much) and also (as in the Wide_Text_IO case),
--  we replace the prefix ada.wide_wide_text_io- by a-zt- and then
--  the normal crunching rules are applied.

--  These are the only irregularity required (so far) to keep the file names
--  unique in the standard predefined libraries.

procedure Krunch
  (Buffer        : in out String;
   Len           : in out Natural;
   Maxlen        : Natural;
   No_Predef     : Boolean;
   VMS_On_Target : Boolean := False);
pragma Elaborate_Body (Krunch);
--  The full file name is stored in Buffer (1 .. Len) on entry. The file
--  name is crunched in place and on return Len is updated, so that the
--  resulting krunched name is in Buffer (1 .. Len) where Len <= Maxlen.
--  Note that Len may be less than or equal to Maxlen on entry, in which
--  case it may be possible that Krunch does not modify Buffer. The fourth
--  parameter, No_Predef, is a switch which, if set to True, disables the
--  normal special treatment of predefined library unit file names.
--  VMS_On_Target, when True, indicates to Krunch to apply the VMS treatment
--  to the children of package A, G,I or S.
--
--  Note: the string Buffer must have a lower bound of 1, and may not
--  contain any blanks (in particular, it must not have leading blanks).
