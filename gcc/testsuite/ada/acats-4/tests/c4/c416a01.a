-- C416A01.A
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
--
--*
-- OBJECTIVE:
--     Check that the name specified for a Constant_Indexing or
--     Variable_Indexing aspect can refer to a set of overloaded functions.
--     (Case A: overloaded on parameters)
--
--     Check that the name specified for a Constant_Indexing or
--     Variable_Indexing aspect can refer to a function with more than
--     two parameters.
--
--     When both Constant_Indexing and Variable_Indexing are both specified,
--     check that when the prefix is a variable, the variable indexing function
--     is called in variable contexts, and the constant indexing function
--     is called in other contexts.
--
-- TEST DESCRIPTION:
--     The foundation is a sprite-drawing package. The main subprogram defines
--     several sprites in a window and tests operations on them.
--
-- CHANGE HISTORY:
--     13 May 2015   RLB   Created test.

with Report;
with F416A00; use F416A00;
with TcTouch;
procedure C416A01 is

    Maze : Window;

begin

    Report.Test ("C416A01",
                 "Check that the name specified for a Constant_Indexing or " &
                 "Variable_Indexing aspect can refer to a set of overloaded " &
                 "functions, with two or more parameters, and that all of " &
                 "those functions can be used in a generalized indexing");

    -- Create sprites:
    Create_Sprite (Maze, "User  ", X=> 10, Y => 10, K => Player, C => Yellow);

    Create_Sprite (Maze, "Blinky", X=> 1, Y => 1, K => Ghost, C => Red);

    Create_Sprite (Maze, "Pinky ", X=> 1, Y => 20, K => Ghost, C => Pink);

    Create_Sprite (Maze, "Inky  ", X=> 20, Y => 20, K => Ghost, C => Cyan);

    Create_Sprite (Maze, "Clyde ", X=> 20, Y => 1, K => Ghost, C => Orange);

    -- Check some sprite properties by name: (CRef(Name))
    if Maze("Blinky").C /= Red then               ------- cB
       Report.Failed ("Wrong color (A)");
    end if;
    if Maze("Clyde ").K /= Ghost then             ------- cC
       Report.Failed ("Wrong kind (A)");
    end if;
    if Maze("User  ").X /= 10 then                ------- cU
       Report.Failed ("Wrong location (A)");
    end if;
    begin
       if Maze("Stinky").Y /= 1 then              ------- cS
          Report.Comment ("Weird");
       end if;
       Report.Failed ("Exception not raised by unknown name (A)");
    exception
       when Missing_Error => null; -- Expected.
    end;

    TcTouch.Validate (Expected => "cBcCcUcS",
                      Message => "Index by name wrong (A)");

    -- Check some sprite properties by position: (CRef(X, Y))
    if Maze(10, 10).C /= Yellow then             ------- dJJ
       Report.Failed ("Wrong color (B)");
    end if;
    if Maze(20, 20).K /= Ghost then              ------- dTT
       Report.Failed ("Wrong kind (B)");
    end if;
    if Maze(1, 1).X /= 1 then                    ------- dAA
       Report.Failed ("Wrong location (B)");
    end if;
    begin
       if Maze(12, 12).Y /= 12 then              ------- dLL
          Report.Comment ("Weird");
       end if;
       Report.Failed ("Exception not raised by unknown location (B)");
    exception
       when Missing_Error => null; -- Expected.
    end;

    TcTouch.Validate (Expected => "dJJdTTdAAdLL",
                      Message => "Index by location wrong (B)");

    -- Change sprite location by name: (VRef(Name))
    Maze("User  ").X := 9;                      ------- vU
    Maze("User  ").Y := 9;                      ------- vU

    -- Change sprite location by location: (VRef(X, Y))
    Maze(20, 20).X := 19;                       ------- wTT
    Maze(19, 20).Y := 19;                       ------- wST

    -- Check results (reversing access):
    if Maze(9, 9).X /= 9 or else                ------- dII
       Maze(9, 9).Y /= 9 or else                ------- dII
       Maze(9, 9).K /= Player then              ------- dII
       Report.Failed ("Change by name failed (C)");
    end if;

    if Maze("Inky  ").X /= 19 or else           ------- cI
       Maze("Inky  ").Y /= 19 or else           ------- cI
       Maze("Inky  ").K /= Ghost then           ------- cI
       Report.Failed ("Change by location failed (C)");
    end if;

    TcTouch.Validate (Expected => "vUvUwTTwSTdIIdIIdIIcIcIcI",
                      Message => "Modifications wrong (C)");

    -- Try calling an indexing in other variable and constant contexts:
    declare
       procedure Use_It (X : in out Natural;
                         Y : in Natural) is
       begin
          if X = Y and then X > Report.Ident_Int(20) then
             Report.Comment ("Shouldn't happen");
             X := 20;
          end if;
       end Use_It;

       S1 : Sprite renames Maze("Clyde ");     ------- vC
       S2 : Sprite renames Maze(20, 1);        ------- wTA

    begin
       Use_It (Maze("Clyde ").X,               ------- vC
               Maze("Clyde ").Y);              ------- cC

       Use_It (Maze(19, 19).X,                 ------- wSS
               Maze(19, 19).Y);                ------- dSS

       case Maze("Pinky ").K is                ------- cP
          when Ghost => null;
          when others => Report.Failed ("Wrong kind (R)");
       end case;

       TcTouch.Validate_One_Of (Expected_1 => "vCwTAvCcCwSSdSScP",
                                Expected_2 => "vCwTAvCcCdSSwSScP",
                                Expected_3 => "vCwTAcCvCwSSdSScP",
                                Expected_4 => "vCwTAcCvCdSSwSScP",
                                Message => "Wrong routine called (E)");
           -- Note: Order of parameter evaluation is unspecified, so
           -- we allow all four possible orders.
    end;

    Report.Result;

end C416A01;
