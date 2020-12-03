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
--    Part 1: Integer, float, and access types, procedures and functions.
--
-- TEST DESCRIPTION:
--    Subtests are:
--        (A) Scalar parameters to procedures.
--        (B) Scalar parameters to functions.
--        (C) Access parameters to procedures.
--        (D) Access parameters to functions.
--
--    For the procedure examples, we pass array elements indexed by dynamically
--    determined indexes. Doing this side-steps the check of 6.4.1(6.15/3) and
--    makes the test more realistic.
--
--    To completely test this objective, we should also try in out and out
--    parameters for functions (Ada 2012), in/in out/out parameters for
--    task and protected entries, and a variety of different scalar types
--    (enumeration, modular, fixed, decimal).
--
-- CHANGE HISTORY:
--    14 Jan 1980 DAS Created test.
--    26 Oct 1982 SPS
--    25 May 1984 CPP
--    29 Oct 1985 EG  Eliminate the use of Numeric_Error in the test.
--    14 Mar 2014 RLB Revised so test cases are legal for Ada 2012, modernized
--                    objective, converted to modern format, added float cases.

with Report;
procedure C620001 is

     use Report;

begin
     Test ("C620001", "Check that elementary parameters are passed by copy");

     --------------------------------------------------

     declare  -- (A)

          I,J,K : Natural := Report.Ident_Int(1); -- Index values.
          Arr   : array (1 .. 4) of Integer;
          E     : exception;

          procedure P (PI  : in     Integer;
                       PO  :    out Integer;
                       PIO : in out Integer) is

               Tmp  : Integer;

          begin

               Tmp := PI;     -- Save value of PI at procedure entry.

               PO := 10;
               if (PI /= Tmp) then
                    Failed ("Assignement to scalar out " &
                            "parameter changes the value of " &
                            "input parameter");
                    Tmp := PI;     -- Reset Tmp for next case.
               end if;

               PIO := PIO + 100;
               if (PI /= Tmp) then
                    Failed ("Assignment to scalar in out " &
                            "parameter changes the value of " &
                            "inputparameter");
                    Tmp := PI;     -- Reset Tmp for next case.
               end if;

               Arr(I) := Arr(I) + 1;
               if (PI /= Tmp) then
                    Failed ("Assignment to scalar actual " &
                            "parameter changes the value of " &
                            "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

     begin  -- (A)
          Arr := (others => 0);
          P (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - A");
     exception
          when E =>
               if (Arr(I) /= 1) then
                    case Arr(I) is
                         when 11  =>
                              Failed ("Out actual scalar parameter " &
                                      "changed global value");
                         when 101 =>
                              Failed ("In out actual scalar " &
                                      "parameter changed global value");
                         when 111 =>
                              Failed ("Out and in out actual scalar " &
                                      "parameters changed global " &
                                      "value");
                         when others =>
                              Failed ("Uundetermined change to global " &
                                      "value");
                    end case;
               end if;
          when others =>
               Failed ("Wrong exception raised - A");
     end;  -- (A)

     --------------------------------------------------

     declare  -- (B)

          I,J  : Integer;

          function F (FI : in Integer) return Integer is

               Tmp  : Integer := FI;

          begin

               I := I + 1;
               if (FI /= Tmp) then
                    Failed ("Assignment to scalar actual function " &
                            "parameter changes the value of " &
                            "input parameter");
               end if;

               return (100);
          end F;

     begin  -- (B)
          I := 100;
          J := F (I);
     end;  -- (B)

     --------------------------------------------------

     declare  -- (C)

          type Acctype is access Integer;

          I,J,K : Natural := Report.Ident_Int(2); -- Index values.
          Arr   : array (1 .. 5) of Acctype;
          E     : exception;

          procedure P (PI  : in     Acctype;
                       PO  :    out Acctype;
                       PIO : in out Acctype) is

               Tmp  : Acctype;

          begin

               Tmp := PI;     -- Save value of PI at procedure entry.

               Arr(I) := new Integer'(101);
               if (PI /= Tmp) then
                    Failed ("Assignment to access actual " &
                            "parameter changes the value of " &
                            "input parameter");
                    Tmp := PI;     -- Reset Tmp for next case.
               end if;

               PO := new Integer'(1);
               if (PI /= Tmp) then
                    Failed ("Assignment to access out " &
                            "parameter changes the value of " &
                            "input parameter");
                    Tmp := PI;     -- Reset Tmp for next case.
               end if;

               PIO := new Integer'(10);
               if (PI /= Tmp) then
                    Failed ("Assignment to access in out " &
                            "parameter changes the value of " &
                            "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

     begin  -- (C)
          Arr(I) := new Integer'(100);
          P (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - C");
     exception
          when E =>
               if (Arr(I).all /= 101) then
                    Failed ("Out or in out actual procedure " &
                            "parameter value changed despite " &
                            "raised exception");
               end if;
          when others =>
               Failed ("Wrong exception raised - C");
     end;  -- (C)

     --------------------------------------------------

     declare  -- (D)

          Type Acctype is access Integer;

          I,J  : Acctype;

          function F (FI : in Acctype) return Acctype is

               Tmp  : Acctype := FI;

          begin

               I := new Integer;
               if (FI /= Tmp) then
                    Failed ("Assignment to access actual function " &
                            "parameter changes the value of " &
                            "Input parameter");
               end if;

               return null;
          end F;

     begin  -- (D)
          I := null;
          J := F(I);
     end;  -- (D)

     --------------------------------------------------

     declare  -- (E)

          I,J,K : Natural := Report.Ident_Int(3); -- Index values.
          Arr   : array (1 .. 3) of Float;
          E     : exception;

          procedure P (PI  : in     Float;
                       PO  :    out Float;
                       PIO : in out Float) is

               Tmp  : Float;

          begin

               Tmp := PI;     -- Save value of PI at procedure entry.

               PO := 0.5;
               if (PI /= Tmp) then
                    Failed ("Assignement to float out " &
                            "parameter changes the value of " &
                            "input parameter");
                    Tmp := PI;     -- Reset Tmp for next case.
               end if;

               PIO := PIO + 0.25;
               if (PI /= Tmp) then
                    Failed ("Assignment to float in out " &
                            "parameter changes the value of " &
                            "inputparameter");
                    Tmp := PI;     -- Reset Tmp for next case.
               end if;

               Arr(I) := Arr(I) + 1.0;
               if (PI /= Tmp) then
                    Failed ("Assignment to float actual " &
                            "parameter changes the value of " &
                            "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

     begin  -- (E)
          Arr := (others => 0.0);
          P (Arr(I), Arr(J), Arr(K));
          Failed ("Exception not raised - E");
     exception
          when E =>
               if (Arr(I) /= 1.0) then
                    Failed ("Out or in out actual procedure " &
                            "parameter value changed despite " &
                            "raised exception");
               end if;
          when others =>
               Failed ("Wrong exception raised - E");
     end;  -- (E)

     --------------------------------------------------

     declare  -- (F)

          I,J  : Float;

          function F (FI : in Float) return Float is

               Tmp  : Float := FI;

          begin

               I := I + 1.0;
               if (FI /= Tmp) then
                    Failed ("Assignment to float actual function " &
                            "parameter changes the value of " &
                            "input parameter");
               end if;

               return 100.0;
          end F;

     begin  -- (F)
          I := 100.0;
          J := F (I);
     end;  -- (F)

     --------------------------------------------------

     Result;

end C620001;
