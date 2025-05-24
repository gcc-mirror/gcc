-- C452A02.A
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
-- OBJECTIVES:
--     Check that "=" for all language-defined nonlimited private types
--     behaves as if it is predefined for composition and for
--     formal types - Part 2: Types from the Ada.Strings subsystem.
--
-- TEST DESCRIPTION:
--     This test also checks for reemergence of some other operation in a
--     generic, and for use in the equality for an array type.
--
--     The foundation is used to do the actual checks; this unit just has
--     to set up appropriate values.
--
--     Notes: We only need to test private types here, as language-defined
--     scalar type equality is tested elsewhere.
--
--     This test could pass even if the type is incorrectly implemented if
--     the predefined equality and the user-defined equality happen to get
--     the same result for the values used in this test. With the function
--     being tested returning only two distinct values, that is definitely
--     possible.
--
--     This test is less likely to fail for a correct Ada 2012 implementation
--     than for an implementation for earlier versions of Ada, as Ada 2012
--     requires all record types to compose properly (this was only true of
--     tagged types in earlier versions).
--
-- CHANGE HISTORY:
--     25 JAN 2001   PHL   Initial version.
--     19 Dec 2018   RLB   Created test from submitted version; added
--                         missing Wide_Wide_ cases.

with Ada.Strings.Maps;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Bounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.Wide_Wide_Unbounded;

with Report;
use Report;
with F452A00;
procedure C452A02 is
begin
    Test ("C452A02",
          "Check that ""="" for all language-defined nonlimited types " &
          "behaves as if it is predefined for composition and for " &
          "formal types - Part 2: Types from the Ada.Strings subsystem");

    Character_Set:
    declare
        package Asm renames Ada.Strings.Maps;

        C1 : constant Asm.Character_Set :=
           Asm.To_Set (Asm.Character_Ranges'((Low => 'a', High => 'm'),
                                             (Low => 'A', High => 'M'),
                                             (Low => '0', High => '3')));
        C2 : Asm.Character_Set :=
           Asm.To_Set (Asm.Character_Range'(Low => 'A', High => 'M'));
        C3 : Asm.Character_Set;

        package Inst is new F452A00 ("Character_Set",
                                       Asm.Character_Set, C1, C2, C3);
    begin
        if Equal (2, 2) then -- Optimization breaker.
            C2 := Asm."or" (C2, Asm.To_Set (Asm.Character_Range'
                                            (Low => 'a', High => 'm')));
            C2 := Asm."or" (C2, Asm.To_Set (Asm.Character_Range'
                                            (Low => '0', High => '3')));
            C3 := Asm.To_Set (Asm.Character_Range'
                              (Low => 'A', High => 'M'));
            C3 := Asm."or" (C3, Asm.To_Set (Asm.Character_Ranges'
                                            ((Low => 'a', High => 'm'),
                                             (Low => '0', High => '4'))));
        else
            C2 := Asm."or" (C2, Asm.To_Set (Asm.Character_Range'
                                            (Low => 'b', High => 'm')));
            C2 := Asm."or" (C2, Asm.To_Set (Asm.Character_Range'
                                            (Low => '0', High => '3')));
            C3 := Asm.To_Set (Asm.Character_Range'
                              (Low => 'B', High => 'M'));
            C3 := Asm."or" (C3, Asm.To_Set (Asm.Character_Ranges'
                                            ((Low => 'b', High => 'm'),
                                             (Low => '0', High => '4'))));
        end if;
        Inst.Check;
    end Character_Set;

    Bounded_String:
    declare

        package Bs is new Ada.Strings.Bounded.Generic_Bounded_Length
                             (Ident_Int (80));

        C1 : constant Bs.Bounded_String :=
           Bs.To_Bounded_String (Ident_Str ("Albert Einstein"));
        C2 : Bs.Bounded_String :=
           Bs.To_Bounded_String (Ident_Str ("Albert"));
        C3 : Bs.Bounded_String :=
           Bs.To_Bounded_String (Ident_Str ("Albert"));

        package Inst is new F452A00 ("Bounded_String",
                                       Bs.Bounded_String, C1, C2, C3);
    begin
        if Equal (3, 3) then -- Optimization breaker.
            Bs.Append (C2, Ident_Char (' '));
            Bs.Append (C3, Ident_Str ("Einstein"));
            Bs.Append (C2, Ident_Str ("Einstein"));
        else
            Bs.Append (C3, Ident_Char (' '));
            Bs.Append (C2, Ident_Str ("Einstein"));
            Bs.Append (C3, Ident_Str ("Einstein"));
        end if;
        Inst.Check;
    end Bounded_String;

    Unbounded_String:
    declare
        package Asu renames Ada.Strings.Unbounded;

        C1 : constant Asu.Unbounded_String :=
           Asu.To_Unbounded_String (Ident_Str ("James Clerk Maxwell"));
        C2 : Asu.Unbounded_String :=
           Asu.To_Unbounded_String (Ident_Str ("James Clerk"));
        C3 : Asu.Unbounded_String :=
           Asu.To_Unbounded_String (Ident_Str ("James Clerk"));

        package Inst is new F452A00 ("Unbounded_String",
                                       Asu.Unbounded_String, C1, C2, C3);
    begin
        if Equal (3, 3) then -- Optimization breaker.
            Asu.Append (C2, Ident_Char (' '));
            Asu.Append (C3, Ident_Str ("Maxwell"));
            Asu.Append (C2, Ident_Str ("Maxwell"));
        else
            Asu.Append (C3, Ident_Char (' '));
            Asu.Append (C2, Ident_Str ("Maxwell"));
            Asu.Append (C3, Ident_Str ("Maxwell"));
        end if;
        Inst.Check;
    end Unbounded_String;

    Wide_Character_Set:
    declare
        package Asw renames Ada.Strings.Wide_Maps;

        C1 : constant Asw.Wide_Character_Set :=
           Asw.To_Set (Asw.Wide_Character_Ranges'
                       ((Low => Wide_Character'Val (123),
                         High => Wide_Character'Val (234)),
                        (Low => Wide_Character'Val (345),
                         High => Wide_Character'Val (456)),
                        (Low => '0', High => '3')));
        C2 : Asw.Wide_Character_Set :=
           Asw.To_Set (Asw.Wide_Character_Range'
                       (Low => Wide_Character'Val (345),
                        High => Wide_Character'Val (456)));
        C3 : Asw.Wide_Character_Set;

        package Inst is new F452A00 ("Wide_Character_Set",
                                       Asw.Wide_Character_Set, C1, C2, C3);
    begin
        if Equal (4, 4) then -- Optimization breaker.
            C2 := Asw."or"
                     (C2, Asw.To_Set (Asw.Wide_Character_Range'
                                      (Low => Wide_Character'Val (123),
                                       High => Wide_Character'Val (234))));
            C2 := Asw."or" (C2, Asw.To_Set (Asw.Wide_Character_Range'
                                            (Low => '0', High => '3')));
            C3 := Asw.To_Set (Asw.Wide_Character_Range'
                              (Low => Wide_Character'Val (345),
                               High => Wide_Character'Val (456)));
            C3 := Asw."or" (C3, Asw.To_Set
                                   (Asw.Wide_Character_Ranges'
                                    ((Low => Wide_Character'Val (123),
                                      High => Wide_Character'Val (234)),
                                     (Low => '0', High => '4'))));
        else
            C2 := Asw."or"
                     (C2, Asw.To_Set (Asw.Wide_Character_Range'
                                      (Low => 'b',
                                       High => Wide_Character'Val (234))));
            C2 := Asw."or" (C2, Asw.To_Set (Asw.Wide_Character_Range'
                                            (Low => '0', High => '3')));
            C3 := Asw.To_Set
                     (Asw.Wide_Character_Range'
                      (Low => 'B', High => Wide_Character'Val (456)));
            C3 := Asw."or"
                     (C3, Asw.To_Set (Asw.Wide_Character_Ranges'
                                      ((Low => 'b',
                                        High => Wide_Character'Val (234)),
                                       (Low => '0', High => '4'))));
        end if;
        Inst.Check;
    end Wide_Character_Set;

    Bounded_Wide_String:
    declare

        package WBs is new Ada.Strings.Wide_Bounded.Generic_Bounded_Length
                             (Ident_Int (80));

        Theta : constant Wide_Character := Wide_Character'Val (16#398#);
        PI    : constant Wide_Character := Wide_Character'Val (16#3A0#);
        Omega : constant Wide_Character := Wide_Character'Val (16#3A9#);

        C1 : constant WBs.Bounded_Wide_String :=
           WBs.To_Bounded_Wide_String ("Greek: " &
                                       Theta & PI & Omega);
        C2 : WBs.Bounded_Wide_String :=
           WBs.To_Bounded_Wide_String ("Greek: ");
        C3 : WBs.Bounded_Wide_String :=
           WBs.To_Bounded_Wide_String ("Greek: ");

        package Inst is new F452A00 ("Bounded_Wide_String",
                                       WBs.Bounded_Wide_String, C1, C2, C3);
    begin
        if Equal (3, 3) then -- Optimization breaker.
            WBs.Append (C3, Omega & Theta & PI);
            WBs.Append (C2, Theta & PI & Omega);
        else
            WBs.Append (C3, Theta & PI & Omega);
            WBs.Append (C2, Theta & Omega & PI);
        end if;
        Inst.Check;
    end Bounded_Wide_String;

    Unbounded_Wide_String:
    declare
        package Aswu renames Ada.Strings.Wide_Unbounded;

        BE  : constant Wide_Character := Wide_Character'Val (16#411#);
        ER  : constant Wide_Character := Wide_Character'Val (16#420#);
        CHE : constant Wide_Character := Wide_Character'Val (16#427#);

        C1 : constant Aswu.Unbounded_Wide_String :=
           Aswu.To_Unbounded_Wide_String ("Cyrillic: " & CHE & ER);
        C2 : Aswu.Unbounded_Wide_String :=
           Aswu.To_Unbounded_Wide_String ("Cyrillic: ");
        C3 : Aswu.Unbounded_Wide_String :=
           Aswu.To_Unbounded_Wide_String ("Cyrillic: ");

        package Inst is new F452A00 ("Unbounded_Wide_String",
                                       Aswu.Unbounded_Wide_String, C1, C2, C3);
    begin
        if Equal (3, 3) then -- Optimization breaker.
            Aswu.Append (C3, BE & ER);
            Aswu.Append (C2, CHE & ER);
        else
            Aswu.Append (C3, CHE & ER);
            Aswu.Append (C2, BE & BE);
        end if;
        Inst.Check;
    end Unbounded_Wide_String;


    Wide_Wide_Character_Set:
    declare
        package Asw renames Ada.Strings.Wide_Wide_Maps;

        C1 : constant Asw.Wide_Wide_Character_Set :=
           Asw.To_Set (Asw.Wide_Wide_Character_Ranges'
                       ((Low => Wide_Wide_Character'Val (16#1F4B4#),
                         High => Wide_Wide_Character'Val (16#1F4B7#)),
                        (Low => Wide_Wide_Character'Val (345),
                         High => Wide_Wide_Character'Val (456)),
                        (Low => '0', High => '3')));
        C2 : Asw.Wide_Wide_Character_Set :=
           Asw.To_Set (Asw.Wide_Wide_Character_Range'
                       (Low => Wide_Wide_Character'Val (345),
                        High => Wide_Wide_Character'Val (456)));
        C3 : Asw.Wide_Wide_Character_Set;

        package Inst is new F452A00 ("Wide_Wide_Character_Set",
                                      Asw.Wide_Wide_Character_Set, C1, C2, C3);
    begin
        if Equal (4, 4) then -- Optimization breaker.
            C2 := Asw."or"
                     (C2, Asw.To_Set (Asw.Wide_Wide_Character_Range'
                                      (Low  =>
                                        Wide_Wide_Character'Val (16#1F4B4#),
                                       High =>
                                        Wide_Wide_Character'Val (16#1F4B7#))));
            C2 := Asw."or" (C2, Asw.To_Set (Asw.Wide_Wide_Character_Range'
                                            (Low => '0', High => '3')));
            C3 := Asw.To_Set (Asw.Wide_Wide_Character_Range'
                              (Low  => Wide_Wide_Character'Val (345),
                               High => Wide_Wide_Character'Val (456)));
            C3 := Asw."or" (C3, Asw.To_Set
                                   (Asw.Wide_Wide_Character_Ranges'
                                    ((Low  =>
                                         Wide_Wide_Character'Val (16#1F4B4#),
                                      High =>
                                         Wide_Wide_Character'Val (16#1F4B7#)),
                                     (Low => '0', High => '4'))));
        else
            C2 := Asw."or"
                     (C2, Asw.To_Set (Asw.Wide_Wide_Character_Range'
                                      (Low  =>
                                        Wide_Wide_Character'Val (16#1F4B4#),
                                       High =>
                                        Wide_Wide_Character'Val (16#1F4B7#))));
            C2 := Asw."or" (C2, Asw.To_Set (Asw.Wide_Wide_Character_Range'
                                            (Low => '0', High => '3')));
            C3 := Asw.To_Set
                     (Asw.Wide_Wide_Character_Range'
                      (Low => 'B', High => Wide_Wide_Character'Val (456)));
            C3 := Asw."or"
                     (C3, Asw.To_Set (Asw.Wide_Wide_Character_Ranges'
                                      ((Low =>
                                          Wide_Wide_Character'Val (16#1F4B4#),
                                       High =>
                                          Wide_Wide_Character'Val (16#1F4B7#)),
                                       (Low => '0', High => '4'))));
        end if;
        Inst.Check;
    end Wide_Wide_Character_Set;

    Bounded_Wide_Wide_String:
    declare

        package WWBs is new Ada.Strings.Wide_Wide_Bounded.
                            Generic_Bounded_Length (Ident_Int (80));

        Poodle : constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val (16#1F429#);
        Poo    : constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val (16#1F49A#);
                 -- The favorite character for Unicode examples.
        Money  : constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val (16#1F4B0#);

        C1 : constant WWBs.Bounded_Wide_Wide_String :=
           WWBs.To_Bounded_Wide_Wide_String ("Great, huh? " &
                                       Poo);
        C2 : WWBs.Bounded_Wide_Wide_String :=
           WWBs.To_Bounded_Wide_Wide_String ("Great, huh? ");
        C3 : WWBs.Bounded_Wide_Wide_String :=
           WWBs.To_Bounded_Wide_Wide_String ("Great, huh? ");

        package Inst is new F452A00 ("Bounded_Wide_Wide_String",
                                      WWBs.Bounded_Wide_Wide_String,
                                      C1, C2, C3);
    begin
        if Equal (3, 3) then -- Optimization breaker.
            WWBs.Append (C3, Poodle);
            WWBs.Append (C2, Poo);
        else
            WWBs.Append (C3, Poodle);
            WWBs.Append (C2, Money);
        end if;
        Inst.Check;
    end Bounded_Wide_Wide_String;

    Unbounded_Wide_Wide_String:
    declare
        package Aswwu renames Ada.Strings.Wide_Wide_Unbounded;

        Poo    : constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val (16#1F49A#);
        Money  : constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val (16#1F4B0#);
        Floppy : constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val (16#1F4BE#);

        C1 : constant Aswwu.Unbounded_Wide_Wide_String :=
           Aswwu.To_Unbounded_Wide_Wide_String ("Worth it " & Money);
        C2 : Aswwu.Unbounded_Wide_Wide_String :=
           Aswwu.To_Unbounded_Wide_Wide_String ("Worth it ");
        C3 : Aswwu.Unbounded_Wide_Wide_String :=
           Aswwu.To_Unbounded_Wide_Wide_String ("Worth it ");

        package Inst is new F452A00 ("Unbounded_Wide_Wide_String",
                                      Aswwu.Unbounded_Wide_Wide_String,
                                      C1, C2, C3);
    begin
        if Equal (3, 3) then -- Optimization breaker.
            Aswwu.Append (C3, Floppy);
            Aswwu.Append (C2, Money);
        else
            Aswwu.Append (C3, Poo);
            Aswwu.Append (C2, Floppy);
        end if;
        Inst.Check;
    end Unbounded_Wide_Wide_String;

    Result;
end C452A02;

