-- C620001.A

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- OBJECTIVE:
--    Check that elementary parameters are passed by copy.
--
--    Part 2: Integer, float, and access types, task and protected entries.
--
-- TEST DESCRIPTION:
--    Subtests are:
--        (A) Scalar parameters to task entries.
--        (B) Scalar parameters to protected entries.
--        (C) Access parameters to task entries.
--        (D) Access parameters to protected entries.
--
--    For all of these examples, we pass array elements indexed by dynamically
--    determined indexes. Doing this side-steps the check of 6.4.1(6.15/3) and
--    makes the test more realistic.
--
--    Note: This is based on legacy test C95072A.ADA (which was withdrawn).
--
-- CHANGE HISTORY:
--    22 Jul 1985 DAS Created test.
--    12 May 2020 RLB Revised so test cases are legal for Ada 2012, modernized
--                    objective, converted to modern format, added float 
--                    and protected cases.

with Report;
procedure C620002 is

     use Report;

begin
     Test ("C620002", "Check that elementary parameters are passed by copy," &
                      " part 2 - task and protected entries");

     --------------------------------------------------

     declare  -- (A)

          I,J,K : Natural := Report.Ident_Int (1); -- Index values.
          Arr   : array (1 .. 4) of Integer;
          E     : exception;

          task TA is
               entry EA (EI  : in     Integer;
                         EO  :    out Integer;
                         EIO : in out Integer);
          end TA;

          task body TA is

               Tmp : Integer;

          begin

               accept EA (EI  : in     Integer;
                          EO  :    out Integer;
                          EIO : in out Integer) do

                    Tmp := EI;     -- Save value of EI at accept.

                    EO := 10;
                    if EI /= Tmp then
                         Failed ("Assignement to scalar out " &
                                 "parameter changes the value of " &
                                 "input parameter - A");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EIO := EIO + 100;
                    if EI /= Tmp then
                         Failed ("Assignment to scalar in out " &
                                 "parameter changes the value of " &
                                 "input parameter - A");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    Arr(I) := Arr(I) + 1;
                    if EI /= Tmp then
                         Failed ("Assignment to scalar actual " &
                                 "parameter changes the value of " &
                                 "input parameter - A");
                    end if;
 
                    raise E;  -- Check exception handling.
               end EA;

          exception
               when others => null;
          end TA;

     begin  -- (A)
          Arr := (others => 0);
          TA.EA (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - A");
     exception
          when E =>
               if Arr(I) /= 1 then
                    case Arr(I) is
                         when 11  =>
                              Failed ("Out actual scalar parameter " &
                                      "changed global value - A");
                         when 101 =>
                              Failed ("In out actual scalar " &
                                      "parameter changed global value - A");
                         when 111 =>
                              Failed ("Out and in out actual scalar " &
                                      "parameters changed global " &
                                      "value - A");
                         when others =>
                              Failed ("Undetermined change to global " &
                                      "value - A");
                    end case;
               end if;
          when others =>
               Failed ("Wrong exception raised - A");
     end;  -- (A)

     --------------------------------------------------

     declare  -- (B)

          I,J,K : Natural := Report.Ident_Int (3); -- Index values.
          Arr   : array (1 .. 5) of Integer;
          E     : exception;

          protected PA is
               entry EA (EI  : in     Integer;
                         EO  :    out Integer;
                         EIO : in out Integer);
          end PA;

          protected body PA is

               entry EA (EI  : in     Integer;
                         EO  :    out Integer;
                         EIO : in out Integer) when True is

                   Tmp : Integer;

               begin

                    Tmp := EI;     -- Save value of EI at entry.

                    EO := 10;
                    if EI /= Tmp then
                         Failed ("Assignement to scalar out " &
                                 "parameter changes the value of " &
                                 "input parameter - B");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EIO := EIO + 100;
                    if EI /= Tmp then
                         Failed ("Assignment to scalar in out " &
                                 "parameter changes the value of " &
                                 "input parameter - B");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    Arr(I) := Arr(I) + 1;
                    if EI /= Tmp then
                         Failed ("Assignment to scalar actual " &
                                 "parameter changes the value of " &
                                 "input parameter - B");
                    end if;
 
                    raise E;  -- Check exception handling.
               end EA;

          end PA;

     begin  -- (B)
          Arr := (others => 0);
          PA.EA (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - B");
     exception
          when E =>
               if Arr(I) /= 1 then
                    case Arr(I) is
                         when 11  =>
                              Failed ("Out actual scalar parameter " &
                                      "changed global value - B");
                         when 101 =>
                              Failed ("In out actual scalar " &
                                      "parameter changed global value - B");
                         when 111 =>
                              Failed ("Out and in out actual scalar " &
                                      "parameters changed global " &
                                      "value - B");
                         when others =>
                              Failed ("Undetermined change to global " &
                                      "value - B");
                    end case;
               end if;
          when others =>
               Failed ("Wrong exception raised - B");
     end;  -- (B)

     --------------------------------------------------

     declare  -- (C)

          type Acctype is access Integer;

          I,J,K : Natural := Report.Ident_Int (2); -- Index values.
          Arr   : array (1 .. 5) of Acctype;
          E     : exception;

          task TB is
               entry EB (EI  : in     Acctype;
                         EO  :    out Acctype;
                         EIO : in out Acctype);
          end TB;

          task body TB is

               Tmp  : Acctype;

          begin

               accept EB (EI  : in     Acctype;
                          EO  :    out Acctype;
                          EIO : in out Acctype) do

                    Tmp := EI;     -- Save value of EI at accept.

                    Arr(I) := new Integer'(101);
                    if EI /= Tmp then
                         Failed ("Assignment to access actual " &
                                 "parameter changes the value of " &
                                 "input parameter - C");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EO := new Integer'(1);
                    if EI /= Tmp then
                         Failed ("Assignment to access out " &
                                 "parameter changes the value of " &
                                 "input parameter - C");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EIO := new Integer'(10);
                    if EI /= Tmp then
                         Failed ("Assignment to access in out " &
                                 "parameter changes the value of " &
                                 "input parameter - C");
                    end if;

                    raise E;  -- Check exception handling.
              end EB;

          exception
               when others => null;
          end TB;

     begin  -- (C)
          Arr(I) := new Integer'(100);
          TB.EB (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - C");
     exception
          when E =>
               if (Arr(I).all /= 101) then
                    Failed ("Out or in out actual " &
                            "parameter value changed despite " &
                            "raised exception - C");
               end if;
          when others =>
               Failed ("Wrong exception raised - C");
     end;  -- (C)

     --------------------------------------------------

     declare  -- (D)

          type Acctype is access Integer;

          I,J,K : Natural := Report.Ident_Int (4); -- Index values.
          Arr   : array (1 .. 6) of Acctype;
          E     : exception;

          protected PB is
               entry EB (EI  : in     Acctype;
                         EO  :    out Acctype;
                         EIO : in out Acctype);
          end PB;

          protected body PB is

               entry EB (EI  : in     Acctype;
                         EO  :    out Acctype;
                         EIO : in out Acctype) when True is

                    Tmp  : Acctype;

               begin
                    Tmp := EI;     -- Save value of EI at entry.

                    Arr(I) := new Integer'(101);
                    if EI /= Tmp then
                         Failed ("Assignment to access actual " &
                                 "parameter changes the value of " &
                                 "input parameter - D");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EO := new Integer'(1);
                    if EI /= Tmp then
                         Failed ("Assignment to access out " &
                                 "parameter changes the value of " &
                                 "input parameter - D");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EIO := new Integer'(10);
                    if EI /= Tmp then
                         Failed ("Assignment to access in out " &
                                 "parameter changes the value of " &
                                 "input parameter - D");
                    end if;

                    raise E;  -- Check exception handling.
               end EB;

          end PB;

     begin  -- (D)
          Arr(I) := new Integer'(100);
          PB.EB (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - D");
     exception
          when E =>
               if (Arr(I).all /= 101) then
                    Failed ("Out or in out actual " &
                            "parameter value changed despite " &
                            "raised exception - D");
               end if;
          when others =>
               Failed ("Wrong exception raised - D");
     end;  -- (D)

     --------------------------------------------------

     declare  -- (E)

          I,J,K : Natural := Report.Ident_Int (3); -- Index values.
          Arr   : array (1 .. 3) of Float;
          E     : exception;

          task TC is
               entry EC (EI  : in     Float;
                         EO  :    out Float;
                         EIO : in out Float);
          end TC;

          task body TC is

               Tmp : Float;

          begin

               accept EC (EI  : in     Float;
                          EO  :    out Float;
                          EIO : in out Float) do

                    Tmp := EI;     -- Save value of EI at accept.

                    EO := 0.5;
                    if EI /= Tmp then
                         Failed ("Assignement to float out " &
                                 "parameter changes the value of " &
                                 "input parameter - E");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EIO := EIO + 0.25;
                    if EI /= Tmp then
                         Failed ("Assignment to float in out " &
                                 "parameter changes the value of " &
                                 "input parameter - E");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    Arr(I) := Arr(I) + 1.0;
                    if EI /= Tmp then
                         Failed ("Assignment to float actual " &
                                 "parameter changes the value of " &
                                 "input parameter - E");
                    end if;
 
                    raise E;  -- Check exception handling.
               end EC;

          exception
               when others => null;
          end TC;

     begin  -- (E)
          Arr := (others => 0.0);
          TC.EC (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - E");
     exception
          when E =>
               if (Arr(I) /= 1.0) then
                    Failed ("Out or in out actual procedure " &
                            "parameter value changed despite " &
                            "raised exception - E");
               end if;
          when others =>
               Failed ("Wrong exception raised - E");
     end;  -- (E)

     --------------------------------------------------

     declare  -- (F)

          I,J,K : Natural := Report.Ident_Int (6); -- Index values.
          Arr   : array (1 .. 7) of Float;
          E     : exception;

          protected PC is
               entry EC (EI  : in     Float;
                         EO  :    out Float;
                         EIO : in out Float);
          end PC;

          protected body PC is

               entry EC (EI  : in     Float;
                         EO  :    out Float;
                         EIO : in out Float) when True is

                    Tmp : Float;

               begin

                    Tmp := EI;     -- Save value of EI at entry.

                    EO := 0.5;
                    if EI /= Tmp then
                         Failed ("Assignement to float out " &
                                 "parameter changes the value of " &
                                 "input parameter - F");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    EIO := EIO + 0.25;
                    if EI /= Tmp then
                         Failed ("Assignment to float in out " &
                                 "parameter changes the value of " &
                                 "input parameter - F");
                         Tmp := EI;     -- Reset Tmp for next case.
                    end if;

                    Arr(I) := Arr(I) + 1.0;
                    if EI /= Tmp then
                         Failed ("Assignment to float actual " &
                                 "parameter changes the value of " &
                                 "input parameter - F");
                    end if;
 
                    raise E;  -- Check exception handling.
               end EC;

          end PC;

     begin  -- (F)
          Arr := (others => 0.0);
          PC.EC (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - F");
     exception
          when E =>
               if (Arr(I) /= 1.0) then
                    Failed ("Out or in out actual procedure " &
                            "parameter value changed despite " &
                            "raised exception - F");
               end if;
          when others =>
               Failed ("Wrong exception raised - F");
     end;  -- (F)

     --------------------------------------------------

     Result;

end C620002;
