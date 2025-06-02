-- CC30004.A

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
--     Check the runtime properties of a generic formal type within an
--     instance are the same as those of the actual type.
--
--     Part 1: Check that a by-copy type is passed by copy within an
--     instance.
--
-- TEST DESCRIPTION:
--    Subtests are:
--        (A) Integer actual type, integer formal type, generic procedure.
--        (B) Integer actual type, integer formal type, generic function.
--        (C) Access actual type, access formal type, generic procedure.
--        (D) Access actual type, access formal type, generic function.
--        (E) Float actual type, float formal type, generic procedure.
--        (F) Float actual type, float formal type, generic function.
--        (G) Enumeration actual type, private formal type, generic procedure.
--        (H) Enumeration actual type, private formal type, generic function.
--
--    For the procedure examples, we pass array elements indexed by dynamically
--    determined indexes. Doing this side-steps the check of 6.4.1(6.15/3) and
--    makes the test more realistic.
--
-- CHANGE HISTORY:
--    07 Aug 1990 - EVB - Test created by Edward V. Berard.
--    16 Oct 1990 - CJJ - Adjusted lines that were too long; reformatted
--                        header to conform to ACVC standards.
--    14 Mar 2014 - RLB - Revised so test cases are legal for Ada 2012,
--                        improved objective to relate to subclause 12.3,
--                        converted to modern format, added float and private
--                        cases.
--    13 Mar 2015 - RLB - Eliminated overlong lines and tab characters.
--

with Report;
procedure CC30004 is

begin
     Report.Test ("CC30004", "Check the runtime properties of a generic " &
                             "formal type within an instance are the same " &
                             "as those of the actual type; specifically, a " &
                             "by-copy actual type is passed by copy");
     --------------------------------------------------

     Integer_and_Proc:

     declare

          -- (A) Integer actual, integer formal, generic procedure.

          type Number is range 0 .. 120;
          I,J,K : Natural := Report.Ident_Int(1); -- Index values.
          Arr   : array (1 .. 4) of Number := (others => 0); -- Initial value.
          E     : exception;

          generic

            type Integer_Item is range <>;

          procedure P (P_In     : in Integer_Item;
                       P_Out    : out Integer_Item;
                       P_In_Out : in out Integer_Item);

          procedure P (P_In     : in Integer_Item;
                       P_Out    : out Integer_Item;
                       P_In_Out : in out Integer_Item) is

               Store  : Integer_Item;

          begin  -- P

               Store := P_In;     -- Save value of OF P_In at proc entry.

               P_Out := 10;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to integer out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               P_In_Out := P_In_Out + 100;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to integer in out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               Arr(K) := Arr(I) + 1;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to integer global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

          procedure New_P is new P (Integer_Item => Number);

     begin  -- Integer_and_Proc
          New_P (P_In     => Arr(I),
                 P_Out    => Arr(J),
                 P_In_Out => Arr(K));

          Report.Failed ("Exception not raised - integer and procedure");
     exception
          when E =>
               if (Arr(K) /= 1) then
                    case Arr(K) is
                         when 11  =>
                              Report.Failed ("Out actual integer " &
                                             "parameter changed global value");
                         when 101 =>
                              Report.Failed ("In out actual integer " &
                                             "parameter changed global value");
                         when 111 =>
                              Report.Failed ("Out and in out actual " &
                                             "Integer parameters changed " &
                                             "global value");
                         when others =>
                              Report.Failed ("Undetermined change to " &
                                             "global value");
                    end case;
               end if;
          when others =>
              Report.Failed ("Wrong exception raised - Integer, procedure");
     end Integer_and_Proc;

     --------------------------------------------------

     Integer_and_Func:

     declare

          -- (B) Integer actual, integer formal, generic function.

          type Number is range 0 .. 101;
          First  : Number;
          Second : Number;

          generic

              type Item is range <>;

          function F (F_In : in Item) return Item;

          function F (F_In : in Item) return Item is

               Store  : Item := F_In;

          begin  -- F

               First := First + 1;
               if (F_In /= Store) then
                    Report.Failed ("Assignment to integer global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               return 100;
          end F;

          function New_F is new F (Item => Number);

     begin  -- Integer_and_Func
          First  := 100;
          Second := New_F (First);
     end Integer_and_Func;

     --------------------------------------------------

     Access_and_Proc:

     declare

          -- (C) Access actual, access formal, generic procedure.

          type Month_Type is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug,
                              Sep, Oct, Nov, Dec);
          type Day_Type is range 1 .. 31;
          type Year_Type is range 1904 .. 2050;
          type Date is record
              Month : Month_Type;
              Day   : Day_Type;
              Year  : Year_Type;
          end record;

          type Date_Access is access Date;

          I,J,K : Natural := Report.Ident_Int(2); -- Index values.

          Date_Array : array (1..3) of Date_Access :=
             (others => new Date'(Month => Dec,
                                  Day   => 12,
                                  Year  => 2012));

          E    : exception;

          generic

            type Item is private;
            type Access_Item is access Item;

          procedure P (P_In     : in     Access_Item;
                       P_Out    : out    Access_Item;
                       P_In_Out : in out Access_Item);

          procedure P (P_In     : in     Access_Item;
                       P_Out    : out    Access_Item;
                       P_In_Out : in out Access_Item) is

               Store  : Access_Item;

          begin  -- P

               Store := P_In;     -- Savevalue of P_In at proc entry.

               Date_Array(I) := new Date'(Year  => 1990,
                                          Day   => 7,
                                          Month => Aug);
               if (P_In /= Store) then
                    Report.Failed ("Assignment to access global " &
                                   "changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               P_Out := new Item;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to access out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               P_In_Out :=  new Item;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to access in out " &
                                   "parameter changes the value of " &
                                   "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

          procedure New_P is new P (Item         => Date,
                                    Access_Item  => Date_access);

     begin  -- Access_and_Proc
          New_P (P_In     => Date_Array(I),
                 P_Out    => Date_Array(J),
                 P_In_Out => Date_Array(K));

          Report.Failed ("Exception not raised - access and procedure");
     exception
          when E =>
               if (Date_Array(J).all /= (Aug, 7, 1990)) then
                    Report.Failed ("Out or in out actual procedure " &
                                   "parameter value changed despite " &
                                   "raised exception - access, procedure");
               end if;
          when others =>
               Report.Failed ("Wrong exception raised - access and procedure");
     end Access_and_Proc;

     --------------------------------------------------

     Access_and_Func:

     declare

          -- (D) Access actual, access formal, generic procedure.

          type Month_Type is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug,
                              Sep, Oct, Nov, Dec);
          type Day_Type is range 1 .. 31;
          type Year_Type is range 1904 .. 2050;
          type Date is record
            Month : Month_Type;
            Day   : Day_Type;
            Year  : Year_Type;
          end record;

          type Date_access is access Date;
          Date_Pointer : Date_access;
          Next_Date    : Date_access;

          generic

            type Item is private;
            type Access_Item is access Item;

          function F (F_In : in Access_Item) return Access_Item;

          function F (F_In : in Access_Item) return Access_Item is

               Store  : Access_Item := F_In;

          begin  -- F

               Date_Pointer := new Date'(Year  => 1990,
                                         Day   => 7,
                                         Month => Aug);
               if (F_In /= Store) then
                    Report.Failed ("Assignment to access global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               return null;
          end F;

          function New_F is new F (Item        => Date,
                                   Access_Item => Date_access);

     begin  -- Access_and_Func
          Date_Pointer := new Date'(Year  => 2014,
                                    Day   => 19,
                                    Month => Mar);
          Next_Date    := New_F (F_In => Date_Pointer);
     end Access_and_Func;

     --------------------------------------------------

     Float_and_Proc:

     declare

          -- (E) Float actual, float formal, generic procedure.

          I,J,K : Natural := Report.Ident_Int(3); -- Index values.
          Arr   : array (1 .. 5) of Float := (others => 0.25);-- Initial value.
          E     : exception;

          generic

            type Float_Item is digits <>;

          procedure P (P_In     : in Float_Item;
                       P_Out     : out Float_Item;
                       P_In_Out : in out Float_Item);

          procedure P (P_In     : in Float_Item;
                       P_Out    : out Float_Item;
                       P_In_Out : in out Float_Item) is

               Store  : Float_Item;

          begin  -- P

               Store := P_In;     -- Save value of OF P_In at proc entry.

               P_Out := 10.5;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to Float out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               P_In_Out := P_In_Out + 20.5;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to Float in out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               Arr(J) := Arr(K) + 1.0;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to Float global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

          procedure New_P is new P (Float_Item => Float);

     begin  -- Float_and_Proc
          New_P (P_In     => Arr(I),
                 P_Out    => Arr(J),
                 P_In_Out => Arr(K));

          Report.Failed ("Exception not raised - Float and procedure");
     exception
          when E =>
               if Arr(K) /= 1.25 then
                    Report.Failed ("Out or in out actual procedure parameter" &
                                   " value changed despite raised exception" &
                                   " - enumeration, procedure");
               end if;
          when others =>
              Report.Failed ("Wrong exception raised - Float, procedure");
     end Float_and_Proc;

     --------------------------------------------------

     Float_and_Func:

     declare

          -- (F) Float actual, float formal, generic function.

          First  : Float;
          Second : Float;

          generic

              type Item is digits <>;

          function F (F_In : in Item) return Item;

          function F (F_In : in Item) return Item is

               Store  : Item := F_In;

          begin  -- F

               First := First + 1.0;
               if (F_In /= Store) then
                    Report.Failed ("Assignment to Float global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               return 40.0;
          end F;

          function New_F is new F (Item => Float);

     begin  -- Float_and_Func
          First  := 25.5;
          Second := New_F (First);
     end Float_and_Func;

     --------------------------------------------------

     Enum_and_Private_and_Proc:

     declare

          -- (G) Enumeration actual, private formal, generic procedure.

          type Color is (Red, Orange, Yellow, Green, Blue, Violet);
          I,J,K : Natural := Report.Ident_Int(4); -- Index values.
          Arr   : array (1 .. 4) of Color := (others => Green); -- Init. value.
          E     : exception;

          generic

            type Private_Item is private;
            Cnst1 : in Private_Item;
            Cnst2 : in Private_Item;

          procedure P (P_In     : in Private_Item;
                       P_Out    : out Private_Item;
                       P_In_Out : in out Private_Item);

          procedure P (P_In     : in Private_Item;
                       P_Out    : out Private_Item;
                       P_In_Out : in out Private_Item) is

               Store  : Private_Item;

          begin  -- P

               Store := P_In;     -- Save value of OF P_In at proc entry.

               P_Out := Cnst1;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to enumeration out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               P_In_Out := Cnst2;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to enumeration in out " &
                                   "parameter changes the value of " &
                                   "input parameter");
                    Store := P_In;     -- Reset Store for next case.
               end if;

               Arr(I) := Blue;
               if (P_In /= Store) then
                    Report.Failed ("Assignment to enumeration global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               raise E;  -- Check exception handling.
          end P;

          procedure New_P is new P (Private_Item => Color,
                                    Cnst1        => Red,
                                    Cnst2        => Violet);

     begin  -- Enum_and_Private_and_Proc
          New_P (P_In     => Arr(I),
                 P_Out    => Arr(J),
                 P_In_Out => Arr(K));

          Report.Failed ("Exception not raised - enumeration and procedure");
     exception
          when E =>
               if Arr(J) /= Blue then
                    Report.Failed ("Out or in out actual procedure " &
                                   "parameter value changed despite raised " &
                                   "exception - enumeration, procedure");
               end if;
          when others =>
              Report.Failed ("Wrong exception raised - enumeration, " &
                             "procedure");
     end Enum_and_Private_and_Proc;

     --------------------------------------------------

     Enum_And_Private_And_Func:

     declare

          -- (B) enumeration actual, enumeration formal, generic function.

          type Color is (Red, Orange, Yellow, Green, Blue, Violet);
          First  : Color;
          Second : Color;

          generic

             type Item is private;
             Cnst : in Item;

          function F (F_In : in Item) return Item;

          function F (F_In : in Item) return Item is

               Store  : Item := F_In;

          begin  -- F

               First := Yellow;
               if (F_In /= Store) then
                    Report.Failed ("Assignment to enumeration global " &
                                   "changes the value of " &
                                   "input parameter");
               end if;

               return Cnst;
          end F;

          function New_F is new F (Item  => Color,
                                   Cnst  => Orange);

     begin  -- Enum_And_Private_And_Func
          First  := Red;
          Second := New_F (First);
     end Enum_And_Private_And_Func;

     Report.Result;

end CC30004;
