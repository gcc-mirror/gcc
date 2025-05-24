-- CXA3005.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--     Check that the character classification functions defined in package
--     Ada.Wide_Characters.Handling produce correct results when provided
--     arguments from Wide_Character.
--
-- TEST DESCRIPTION:
--     This test checks the character classification functions of package
--     Ada.Wide_Characters.Handling.  A loop is constructed to test the
--     functions for the first 256 positions of Wide_Character,
--     core Greek and Cyrillic characters, and a few selected other characters.
--     (Testing all values of Wide_Character would require a file of almost
--     3 Mbytes and take the best part of 2 hours to compile on a high-end PC.
--     There are around a million combinations of wide character and
--     classification; testing all of them would be open to lots of disputes,
--     especially as the classifications of some characters depend on the
--     character set standard in use).
--
-- CHANGE HISTORY:
--      21 May 13   JAC     Initial pre-release version.
--      10 Jul 13   JAC     Second pre-release version.
--      20 Mar 14   RLB     Readied to issue; renamed, improved objective.
--!
with Ada.Characters.Conversions;
with Ada.Wide_Characters.Handling;
with Report;

procedure CXA3005 is

   type Classifications_Type is
     (Control, Letter, Lower, Upper, Digit, Hexadecimal_Digit, Alphanumeric,
      Special, Line_Terminator, Mark, Other_Format, Punctuation_Connector,
      Space, Graphic);

   type Classifications_Set_Type is array (Classifications_Type) of
     Boolean
     with Pack;

   function F return Boolean renames False;

   function T return Boolean renames True;

   subtype Wide_Character_Range is Wide_Character range Wide_Character'First ..
    Wide_Character'Last;

   type Classifications_Array_Type is array (Wide_Character_Range range <>) of
     Classifications_Set_Type;


   subtype Character_Wide_Character_Subset is Wide_Character range
     Wide_Character'First ..
     Ada.Characters.Conversions.To_Wide_Character (Character'Last);

   Character_Classifications_Array : constant Classifications_Array_Type
    (Character_Wide_Character_Subset) :=
    (
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#0#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#2#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#3#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#4#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#5#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#6#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#7#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9#
     (T,F,F,F,F,F,F,F,T,F,F,F,F,F), -- 16#A#
     (T,F,F,F,F,F,F,F,T,F,F,F,F,F), -- 16#B#
     (T,F,F,F,F,F,F,F,T,F,F,F,F,F), -- 16#C#
     (T,F,F,F,F,F,F,F,T,F,F,F,F,F), -- 16#D#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#E#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#F#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#10#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#11#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#12#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#13#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#14#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#15#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#16#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#17#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#18#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#19#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1A#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1B#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1C#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1D#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1E#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#1F#
     (F,F,F,F,F,F,F,T,F,F,F,F,T,T), -- 16#20#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#21#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#22#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#23#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#24#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#25#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#26#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#27#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#28#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#29#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#2A#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#2B#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#2C#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#2D#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#2E#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#2F#
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 0
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 1
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 2
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 3
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 4
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 5
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 6
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 7
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 8
     (F,F,F,F,T,T,T,F,F,F,F,F,F,T), -- 9
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#3A#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#3B#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#3C#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#3D#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#3E#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#3F#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#40#
     (F,T,F,T,F,T,T,F,F,F,F,F,F,T), -- A
     (F,T,F,T,F,T,T,F,F,F,F,F,F,T), -- B
     (F,T,F,T,F,T,T,F,F,F,F,F,F,T), -- C
     (F,T,F,T,F,T,T,F,F,F,F,F,F,T), -- D
     (F,T,F,T,F,T,T,F,F,F,F,F,F,T), -- E
     (F,T,F,T,F,T,T,F,F,F,F,F,F,T), -- F
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- G
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- H
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- I
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- J
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- K
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- L
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- M
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- N
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- O
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- P
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Q
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- R
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- S
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- T
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- U
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- V
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- W
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- X
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Y
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Z
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#5B#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#5C#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#5D#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#5E#
     (F,F,F,F,F,F,F,T,F,F,F,T,F,T), -- 16#5F#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#60#
     (F,T,T,F,F,T,T,F,F,F,F,F,F,T), -- a
     (F,T,T,F,F,T,T,F,F,F,F,F,F,T), -- b
     (F,T,T,F,F,T,T,F,F,F,F,F,F,T), -- c
     (F,T,T,F,F,T,T,F,F,F,F,F,F,T), -- d
     (F,T,T,F,F,T,T,F,F,F,F,F,F,T), -- e
     (F,T,T,F,F,T,T,F,F,F,F,F,F,T), -- f
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- g
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- h
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- i
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- j
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- k
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- l
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- m
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- n
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- o
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- p
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- q
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- r
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- s
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- t
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- u
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- v
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- w
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- x
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- y
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- z
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#7B#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#7C#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#7D#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#7E#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#7F#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#80#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#81#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#82#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#83#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#84#
     (T,F,F,F,F,F,F,F,T,F,F,F,F,F), -- 16#85#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#86#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#87#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#88#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#89#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8A#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8B#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8C#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8D#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8E#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#8F#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#90#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#91#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#92#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#93#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#94#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#95#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#96#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#97#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#98#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#99#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9A#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9B#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9C#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9D#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9E#
     (T,F,F,F,F,F,F,F,F,F,F,F,F,F), -- 16#9F#
     (F,F,F,F,F,F,F,T,F,F,F,F,T,T), -- 16#A0#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A1#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A2#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A3#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A4#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A5#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A6#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A7#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A8#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#A9#
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ª
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#AB#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#AC#
     (F,F,F,F,F,F,F,T,F,F,T,F,F,T), -- 16#AD#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#AE#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#AF#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B0#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B1#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B2#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B3#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B4#
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- µ
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B6#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B7#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B8#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#B9#
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- º
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#BB#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#BC#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#BD#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#BE#
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#BF#
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- À
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Á
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Â
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ã
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ä
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Å
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Æ
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ç
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- È
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- É
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ê
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ë
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ì
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Í
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Î
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ï
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ð
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ñ
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ò
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ó
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ô
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Õ
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ö
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#D7#
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ø
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ù
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ú
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Û
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ü
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Ý
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- Þ
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ß
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- à
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- á
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- â
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ã
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ä
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- å
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- æ
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ç
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- è
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- é
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ê
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ë
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ì
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- í
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- î
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ï
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ð
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ñ
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ò
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ó
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ô
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- õ
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ö
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T), -- 16#F7#
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ø
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ù
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ú
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- û
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ü
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ý
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- þ
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T)  -- ÿ
    );


   Large_Dotted_I_Classifications_Set     : constant Classifications_Set_Type :=
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T); -- ["0130"]
   Small_Dotless_I_Classifications_Set    : constant Classifications_Set_Type :=
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T); -- ["0131"]


   -- Capital Alpha to Capital Rho
   subtype Core_Greek_1_Wide_Character_Subset is Wide_Character range
     Wide_Character'Val (16#391#) .. Wide_Character'Val (16#3A1#);

   Core_Greek_1_Classifications_Array : constant Classifications_Array_Type
    (Core_Greek_1_Wide_Character_Subset) :=
    (
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0391"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0392"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0393"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0394"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0395"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0396"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0397"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0398"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0399"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["039A"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["039B"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["039C"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["039D"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["039E"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["039F"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A0"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T)  -- ["03A1"]
    );

   -- Note that Final Sigma is small only

   -- Capital Sigma to Capital Omega
   subtype Core_Greek_2_Wide_Character_Subset is Wide_Character range
     Wide_Character'Val (16#3A3#) .. Wide_Character'Val (16#3A9#);

   Core_Greek_2_Classifications_Array : constant Classifications_Array_Type
    (Core_Greek_2_Wide_Character_Subset) :=
    (
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A3"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A4"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A5"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A6"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A7"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["03A8"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T)  -- ["03A9"]
    );

   -- Small Alpha to Small Omega
   subtype Core_Greek_3_Wide_Character_Subset is Wide_Character range
     Wide_Character'Val (16#3B1#) .. Wide_Character'Val (16#3C9#);

   Core_Greek_3_Classifications_Array : constant Classifications_Array_Type
    (Core_Greek_3_Wide_Character_Subset) :=
    (
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B1"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B2"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B3"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B4"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B5"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B6"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B7"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B8"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03B9"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03BA"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03BB"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03BC"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03BD"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03BE"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03BF"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C0"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C1"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C2"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C3"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C4"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C5"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C6"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C7"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["03C8"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T)  -- ["03C9"]
    );


   subtype Core_Cyrillic_Wide_Character_Subset is Wide_Character range
     Wide_Character'Val (16#400#) .. Wide_Character'Val (16#45F#);

   Core_Cyrillic_Classifications_Array : constant
    Classifications_Array_Type (Core_Cyrillic_Wide_Character_Subset) :=
    (
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0400"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0401"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0402"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0403"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0404"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0405"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0406"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0407"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0408"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0409"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["040A"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["040B"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["040C"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["040D"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["040E"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["040F"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0410"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0411"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0412"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0413"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0414"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0415"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0416"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0417"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0418"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0419"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["041A"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["041B"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["041C"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["041D"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["041E"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["041F"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0420"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0421"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0422"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0423"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0424"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0425"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0426"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0427"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0428"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["0429"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["042A"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["042B"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["042C"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["042D"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["042E"]
     (F,T,F,T,F,F,T,F,F,F,F,F,F,T), -- ["042F"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0430"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0431"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0432"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0433"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0434"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0435"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0436"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0437"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0438"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0439"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["043A"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["043B"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["043C"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["043D"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["043E"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["043F"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0440"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0441"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0442"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0443"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0444"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0445"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0446"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0447"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0448"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0449"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["044A"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["044B"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["044C"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["044D"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["044E"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["044F"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0450"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0451"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0452"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0453"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0454"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0455"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0456"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0457"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0458"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["0459"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["045A"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["045B"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["045C"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["045D"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T), -- ["045E"]
     (F,T,T,F,F,F,T,F,F,F,F,F,F,T)  -- ["045F"]
    );


   Hebrew_Alef_Classifications_Set        : constant Classifications_Set_Type :=
     (F,T,F,F,F,F,T,F,F,F,F,F,F,T); -- ["05D0"]


   EN_Dash_Classifications_Set            : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2013#

   EM_Dash_Classifications_Set            : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2014#


   Left_Single_Quote_Classifications_Set  : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2018#

   Right_Single_Quote_Classifications_Set : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2019#

   Left_Double_Quote_Classifications_Set  : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#201C#

   Right_Double_Quote_Classifications_Set : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#201D#


   For_All_Classifications_Set            : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2200#

   There_Exists_Classifications_Set       : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2203#


   Infinity_Classifications_Set           : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#221E#


   Left_Ceiling_Classifications_Set       : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2308#

   Right_Ceiling_Classifications_Set      : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#2309#

   Left_Floor_Classifications_Set         : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#230A#

   Right_Floor_Classifications_Set        : constant Classifications_Set_Type :=
     (F,F,F,F,F,F,F,T,F,F,F,F,F,T); -- 16#230B#


   procedure Test_Character
     (WC                  : in Wide_Character;
      Classifications_Set : in Classifications_Set_Type) is
   begin

      if Ada.Wide_Characters.Handling.Is_Control (WC)/=
        Classifications_Set (Control) then

         Report.Failed
           ("Wrong Is_Control for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Letter (WC)/=
        Classifications_Set (Letter) then

         Report.Failed
           ("Wrong Is_Letter for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Lower (WC)/=
        Classifications_Set (Lower) then

         Report.Failed
           ("Wrong Is_Lower for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Upper (WC)/=
        Classifications_Set (Upper) then

         Report.Failed
           ("Wrong Is_Upper for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

     if Ada.Wide_Characters.Handling.Is_Digit (WC)/=
        Classifications_Set (Digit) then

         Report.Failed
           ("Wrong Is_Digit for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      -- Note Is_Decimal_Digit is a renaming of Is_Digit so should return the
      -- same value.
      if Ada.Wide_Characters.Handling.Is_Decimal_Digit (WC)/=
        Classifications_Set (Digit) then

         Report.Failed
           ("Wrong Is_Decimal_Digit for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

     if Ada.Wide_Characters.Handling.Is_Hexadecimal_Digit (WC)/=
        Classifications_Set (Hexadecimal_Digit) then

         Report.Failed
           ("Wrong Is_Hexadecimal_Digit for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

     if Ada.Wide_Characters.Handling.Is_Alphanumeric (WC)/=
        Classifications_Set (Alphanumeric) then

         Report.Failed
           ("Wrong Is_Alphanumeric for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

     if Ada.Wide_Characters.Handling.Is_Special (WC)/=
        Classifications_Set (Special) then

         Report.Failed
           ("Wrong Is_Special for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

     if Ada.Wide_Characters.Handling.Is_Line_Terminator (WC)/=
        Classifications_Set (Line_Terminator) then

         Report.Failed
           ("Wrong Is_Line_Terminator for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

     if Ada.Wide_Characters.Handling.Is_Mark (WC)/=
        Classifications_Set (Mark) then

         Report.Failed
           ("Wrong Is_Mark for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Other_Format (WC)/=
        Classifications_Set (Other_Format) then

         Report.Failed
           ("Wrong Is_Other_Format for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Punctuation_Connector (WC)/=
        Classifications_Set (Punctuation_Connector) then

         Report.Failed
           ("Wrong Is_Punctuation_Connector for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Space (WC)/=
        Classifications_Set (Space) then

         Report.Failed
           ("Wrong Is_Space for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

      if Ada.Wide_Characters.Handling.Is_Graphic (WC)/=
        Classifications_Set (Graphic) then

         Report.Failed
           ("Wrong Is_Graphic for position " &
            Integer'Image (Wide_Character'Pos (WC)));

      end if;

   end Test_Character;

begin

   Report.Test
     ("CXA3005",
      "Check that the character classification functions defined in package " &
      "Ada.Wide_Characters.Handling produce correct results when provided "   &
      "arguments from Wide_Character");

   for I in Character_Wide_Character_Subset loop

     Test_Character
        (WC                  => I,
         Classifications_Set => Character_Classifications_Array (I));

   end loop;

   Test_Character
      (WC                  => Wide_Character'Val (16#130#), -- Large_Dotted_I
       Classifications_Set => Large_Dotted_I_Classifications_Set);

   Test_Character
      (WC                  => Wide_Character'Val (16#131#), -- Small_Dotless_I
       Classifications_Set => Small_Dotless_I_Classifications_Set);

   for I in Core_Greek_1_Wide_Character_Subset loop

     Test_Character
        (WC                  => I,
         Classifications_Set => Core_Greek_1_Classifications_Array (I));

   end loop;

   for I in Core_Greek_2_Wide_Character_Subset loop

     Test_Character
        (WC                  => I,
         Classifications_Set => Core_Greek_2_Classifications_Array (I));

   end loop;

   for I in Core_Greek_3_Wide_Character_Subset loop

     Test_Character
        (WC                  => I,
         Classifications_Set => Core_Greek_3_Classifications_Array (I));

   end loop;

   for I in Core_Cyrillic_Wide_Character_Subset loop

     Test_Character
        (WC                  => I,
         Classifications_Set => Core_Cyrillic_Classifications_Array (I));

   end loop;

   -- Hebrew_Alef
   Test_Character
      (WC                  => Wide_Character'Val (16#5D0#),
       Classifications_Set => Hebrew_Alef_Classifications_Set);

   -- EN_Dash
   Test_Character
      (WC                  => Wide_Character'Val (16#2013#),
       Classifications_Set => EN_Dash_Classifications_Set);

   -- EM_Dash
   Test_Character
      (WC                  => Wide_Character'Val (16#2014#),
       Classifications_Set => EM_Dash_Classifications_Set);

   -- Left_Single_Quote
   Test_Character
      (WC                  => Wide_Character'Val (16#2018#),
       Classifications_Set => Left_Single_Quote_Classifications_Set);

   -- Right_Single_Quote
   Test_Character
      (WC                  => Wide_Character'Val (16#2019#),
       Classifications_Set => Right_Single_Quote_Classifications_Set);

   -- Left_Double_Quote
   Test_Character
      (WC                  => Wide_Character'Val (16#201C#),
       Classifications_Set => Left_Double_Quote_Classifications_Set);

   -- Right_Double_Quote
   Test_Character
      (WC                  => Wide_Character'Val (16#201D#),
       Classifications_Set => Right_Double_Quote_Classifications_Set);

   -- For_All
   Test_Character
      (WC                  => Wide_Character'Val (16#2200#),
       Classifications_Set => For_All_Classifications_Set);

   -- There_Exists
   Test_Character
      (WC                  => Wide_Character'Val (16#2203#),
       Classifications_Set => There_Exists_Classifications_Set);

   -- Infinity
   Test_Character
      (WC                  => Wide_Character'Val (16#221E#),
       Classifications_Set => Infinity_Classifications_Set);

   -- Left_Ceiling
   Test_Character
      (WC                  => Wide_Character'Val (16#2308#),
       Classifications_Set => Left_Ceiling_Classifications_Set);

   -- Right_Ceiling
   Test_Character
      (WC                  => Wide_Character'Val (16#2309#),
       Classifications_Set => Right_Ceiling_Classifications_Set);

   -- Left_Floor
   Test_Character
      (WC                  => Wide_Character'Val (16#230A#),
       Classifications_Set => Left_Floor_Classifications_Set);

   -- Right_Floor
   Test_Character
      (WC                  => Wide_Character'Val (16#230B#),
       Classifications_Set => Right_Floor_Classifications_Set);

   Report.Result;

end CXA3005;
