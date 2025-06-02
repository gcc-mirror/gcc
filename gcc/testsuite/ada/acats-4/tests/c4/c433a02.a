-- C433A02.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--*
--  OBJECTIVE:
--      Check that the array component expressions in an array aggregate
--      may have a limited type. (Two-dimensional arrays.)
--
--      Check that for each association with a <> in an array aggregate,
--      the component is initialized by default. (Task types, protected
--      types, limited record types, for two-dimensional arrays.)
--
--  TEST DESCRIPTION:
--      This test declares a record type with three components (one protected
--      object, one task, and one limited record) and uses it to declare
--      two-dimensional arrays.
--      The foundation provides task and protected types with support to set
--      and get one value, and thus check that these components have been
--      properly initialized.
--
--  CHANGE HISTORY:
--      2 Feb 2004 JM  Initial Version.
--     19 Sep 2007 RLB Split out foundation to reduce duplicate code,
--                     added cases, and made test self-checking.
--
--!
with Report;
with F433A00; use F433A00;
procedure C433A02 is

   type TR is array (Positive range <>, Positive range <>) of My_Rec;
   subtype STR is TR (1 .. 2, 1 .. 2);

   procedure Check (A : in out TR; Value : in Integer; Message : in String) is
   begin
      for I in A'Range (1) loop
         for J in A'Range (2) loop

             F433A00.Check (A (I, J), Value => Value,
                 Message => Message & "(" & Integer'Image(I) & ","
                                          & Integer'Image(J) & ")");
         end loop;
      end loop;
   end Check;

begin
   Report.Test ("C433A02", "Check that the array component expressions in an " &
                           "array aggregate may have a limited type. " &
                           "Part B: Two-dimensional arrays");

   F433A00.Reset_Init;
   declare
      O_10 : STR := (1 => (1 => <>, 2 => <>),
                    2 => (1 => <>, 2 => <>));
      O_11 : STR := (1 .. 2 => (1 .. 2 => <>));
      O_12 : STR := (1 | 2 => (1 | 2 => <>));
      O_13 : TR (1 .. 3, 1 ..3) := (others => (others => <>));
   begin
      Check (O_10, 4, "O_10");
      Check (O_11, 4, "O_11");
      Check (O_12, 4, "O_12");
      Check (O_13, 4, "O_13");
      F433A00.Check_Init_Count (Expected => 21, Message => "Subtest 1");
   end;

   F433A00.Reset_Init;
   declare
      O_20 : TR (1 .. 2, 1 .. 3) :=
           (1 => (1 => (Info => (A => 2, C => 'A'),
                        P    => <>,
                        T    => <>),
                  2 => (Info => (A => 2, C => 'B'),
                        P    => <>,
                        T    => <>),
                  3 => (Info => (A => 2, C => 'C'),
                        P    => <>,
                        T    => <>)),
            2 => (1 => (Info => (A => 2, C => 'D'),
                        P    => <>,
                        T    => <>),
                  2 => (Info => (A => 2, others => <>),
                        P    => <>,
                        T    => <>),
                  3 => (Info => (A => 2, C => <>),
                        P    => <>,
                        T    => <>)));
      O_21 : STR := (1 .. 2 => (1 .. 2 => (Info => (A => <>, C => 'E'),
                                           P    => <>,
                                           T    => <>)));
      O_22 : STR := (1 | 2 => (1 | 2 => (Info => (A => <>, C => 'F'),
                                         P    => <>,
                                         T    => <>)));
      O_23 : TR(1 .. 3, 1 .. 2) :=
                    (others => (others => (Info => <>,
                                           P    => <>,
                                           T    => <>)));
   begin
      Check (O_20, 2, "O_20");
      Check (O_21, 4, "O_21");
      Check (O_22, 4, "O_22");
      Check (O_23, 4, "O_23");
      F433A00.Check_Init_Count (Expected => 14, Message => "Subtest 2");
   end;

   F433A00.Reset_Init;
   declare
      O_31 : STR := (1 => (others => (Info => <>,
                                      P    => <>,
                                      T    => <>)),
                    2 => (others => <>));

      O_32 : STR := (1 => (others => (Info => <>,
                                      P    => <>,
                                      T    => <>)),
                    others => (others => <>));
   begin
      Check (O_31, 4, "O_31");
      Check (O_32, 4, "O_32");
      F433A00.Check_Init_Count (Expected => 8, Message => "Subtest 3");
   end;

   Report.Result;
end C433A02;
