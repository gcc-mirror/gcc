-- C380003.A
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
--*
--
-- OBJECTIVE:
--    Check that per-object expressions are evaluated as specified for
--    protected components.  (Defect Report 8652/0002, as reflected in
--    Technical Corrigendum 1, RM95 3.6(22/1) and 3.8(18/1)).
--
-- CHANGE HISTORY:
--     9 FEB 2001   PHL   Initial version.
--    29 JUN 2002   RLB   Readied for release.
--
--!
with Report;
use Report;
procedure C380003 is

    subtype Sm is Integer range 1 .. 10;

    type Rec (D1, D2 : Sm) is
        record
            null;
        end record;

begin
    Test ("C380003",
          "Check compatibility of discriminant expressions" &
             " when the constraint depends on discriminants, " &
             "and the discriminants have defaults - protected components");

    declare
        protected type Cons (D3 : Integer := Ident_Int (11)) is
            function C1_D1 return Integer;
            function C1_D2 return Integer;
        private
            C1 : Rec (D3, 1);
        end Cons;
        protected body Cons is
            function C1_D1 return Integer is
            begin
                return C1.D1;
            end C1_D1;
            function C1_D2 return Integer is
            begin
                return C1.D2;
            end C1_D2;
        end Cons;

        function Is_Ok
                    (C : Cons; D3 : Integer; C1_D1 : Integer; C1_D2 : Integer)
                    return Boolean is
        begin
            return C.D3 = D3 and C.C1_D1 = C1_D1 and C.C1_D2 = C1_D2;
        end Is_Ok;

    begin
        begin
            declare
                X : Cons;
            begin
                Failed ("Discriminant check not performed - 1");
                if not Is_Ok (X, 1, 1, 1) then
                    Comment ("Shouldn't get here");
                end if;
            end;
        exception
            when Constraint_Error =>
                null;
            when others =>
                Failed ("Unexpected exception - 1");
        end;

        begin
            declare
                type Acc_Cons is access Cons;
                X : Acc_Cons;
            begin
                X := new Cons;
                Failed ("Discriminant check not performed - 2");
                begin
                    if not Is_Ok (X.all, 1, 1, 1) then
                        Comment ("Irrelevant");
                    end if;
                end;
            exception
                when Constraint_Error =>
                    null;
                when others =>
                    Failed ("Unexpected exception raised - 2");
            end;
        exception
            when others =>
                Failed ("Constraint checked too soon - 2");
        end;

        begin
            declare
                subtype Scons is Cons;
            begin
                declare
                    X : Scons;
                begin
                    Failed ("Discriminant check not performed - 3");
                    if not Is_Ok (X, 1, 1, 1) then
                        Comment ("Irrelevant");
                    end if;
                end;
            exception
                when Constraint_Error =>
                    null;
                when others =>
                    Failed ("Unexpected exception raised - 3");
            end;
        exception
            when others =>
                Failed ("Constraint checked too soon - 3");
        end;

        begin
            declare
                type Arr is array (1 .. 5) of Cons;
            begin
                declare
                    X : Arr;
                begin
                    Failed ("Discriminant check not performed - 4");
                    for I in Arr'Range loop
                        if not Is_Ok (X (I), 1, 1, 1) then
                            Comment ("Irrelevant");
                        end if;
                    end loop;
                end;
            exception
                when Constraint_Error =>
                    null;
                when others =>
                    Failed ("Unexpected exception raised - 4");
            end;
        exception
            when others =>
                Failed ("Constraint checked too soon - 4");
        end;

        begin
            declare
                type Nrec is
                    record
                        C1 : Cons;
                    end record;
            begin
                declare
                    X : Nrec;
                begin
                    Failed ("Discriminant check not performed - 5");
                    if not Is_Ok (X.C1, 1, 1, 1) then
                        Comment ("Irrelevant");
                    end if;
                end;
            exception
                when Constraint_Error =>
                    null;
                when others =>
                    Failed ("Unexpected exception raised - 5");
            end;
        exception
            when others =>
                Failed ("Constraint checked too soon - 5");
        end;

        begin
            declare
                type Drec is new Cons;
            begin
                declare
                    X : Drec;
                begin
                    Failed ("Discriminant check not performed - 6");
                    if not Is_Ok (Cons (X), 1, 1, 1) then
                        Comment ("Irrelevant");
                    end if;
                end;
            exception
                when Constraint_Error =>
                    null;
                when others =>
                    Failed ("Unexpected exception raised - 6");
            end;
        exception
            when others =>
                Failed ("Constraint checked too soon - 6");
        end;

    end;

    Result;

exception
    when others =>
        Failed ("Constraint check done too early");
        Result;
end C380003;
