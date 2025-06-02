-- C380004.A
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
--    Check that per-object expressions are evaluated as specified for entry
--    families and protected components.  (Defect Report 8652/0002,
--    as reflected in Technical Corrigendum 1, RM95 3.6(22/1), 3.8(18/1), and
--    9.5.2(22/1)).
--
-- CHANGE HISTORY:
--     9 FEB 2001   PHL   Initial version.
--    29 JUN 2002   RLB   Readied for release.
--
--!
with Report;
use Report;
procedure C380004 is

    type Rec (D1, D2 : Positive) is
        record
            null;
        end record;

    F1_Poe : Integer;

    function Chk (Poe : Integer; Value : Integer; Message : String)
                 return Boolean is
    begin
        if Poe /= Value then
            Failed (Message & ": Poe is " & Integer'Image (Poe));
        end if;
        return True;
    end Chk;

    function F1 return Integer is
    begin
        F1_Poe := F1_Poe - Ident_Int (1);
        return F1_Poe;
    end F1;

    generic
        type T is limited private;
        with function Is_Ok (X : T;
                             Param1 : Integer;
                             Param2 : Integer;
                             Param3 : Integer) return Boolean;
    procedure Check;

    procedure Check is
    begin

        declare
            type Poe is new T;
            Chk1 : Boolean := Chk (F1_Poe, 17, "F1 evaluated");
            X : Poe;             -- F1 evaluated
            Y : Poe;             -- F1 evaluated
            Chk2 : Boolean := Chk (F1_Poe, 15, "F1 not evaluated");
        begin
            if not Is_Ok (T (X), 16, 16, 17) or
               not Is_Ok (T (Y), 15, 15, 17) then
                Failed ("Discriminant values not correct - 0");
            end if;
        end;

        declare
            type Poe is new T;
        begin
            begin
                declare
                    X : Poe;
                begin
                    if not Is_Ok (T (X), 14, 14, 17) then
                        Failed ("Discriminant values not correct - 1");
                    end if;
                end;
            exception
                when others =>
                    Failed ("Unexpected exception - 1");
            end;

            declare
                type Acc_Poe is access Poe;
                X : Acc_Poe;
            begin
                X := new Poe;
                begin
                    if not Is_Ok (T (X.all), 13, 13, 17) then
                        Failed ("Discriminant values not correct - 2");
                    end if;
                end;
            exception
                when others =>
                    Failed ("Unexpected exception raised - 2");
            end;

            declare
                subtype Spoe is Poe;
                X : Spoe;
            begin
                if not Is_Ok (T (X), 12, 12, 17) then
                    Failed ("Discriminant values not correct - 3");
                end if;
            exception
                when others =>
                    Failed ("Unexpected exception raised - 3");
            end;

            declare
                type Arr is array (1 .. 2) of Poe;
                X : Arr;
            begin
                if Is_Ok (T (X (1)), 11, 11, 17) and then
                   Is_Ok (T (X (2)), 10, 10, 17) then
                    null;
                elsif Is_Ok (T (X (2)), 11, 11, 17) and then
                      Is_Ok (T (X (1)), 10, 10, 17) then
                    null;
                else
                    Failed ("Discriminant values not correct - 4");
                end if;
            exception
                when others =>
                    Failed ("Unexpected exception raised - 4");
            end;

            declare
                type Nrec is
                    record
                        C1, C2 : Poe;
                    end record;
                X : Nrec;
            begin
                if Is_Ok (T (X.C1), 8, 8, 17) and then
                   Is_Ok (T (X.C2), 9, 9, 17) then
                    null;
                elsif Is_Ok (T (X.C2), 8, 8, 17) and then
                      Is_Ok (T (X.C1), 9, 9, 17) then
                    null;
                else
                    Failed ("Discriminant values not correct - 5");
                end if;
            exception
                when others =>
                    Failed ("Unexpected exception raised - 5");
            end;

            declare
                type Drec is new Poe;
                X : Drec;
            begin
                if not Is_Ok (T (X), 7, 7, 17) then
                    Failed ("Discriminant values not correct - 6");
                end if;
            exception
                when others =>
                    Failed ("Unexpected exception raised - 6");
            end;
        end;
    end Check;


begin
    Test ("C380004",
          "Check evaluation of discriminant expressions " &
             "when the constraint depends on a discriminant, " &
             "and the discriminants have defaults - discriminant-dependent" &
             "entry families and protected components");


    Comment ("Discriminant-dependent entry families for task types");

    F1_Poe := 18;

    declare
        task type Poe (D3 : Positive := F1) is
            entry E (D3 .. F1);    -- F1 evaluated
            entry Is_Ok (D3 : Integer;
                         E_First : Integer;
                         E_Last : Integer;
                         Ok : out Boolean);
        end Poe;
        task body Poe is
        begin
            loop
                select
                    accept Is_Ok (D3 : Integer;
                                  E_First : Integer;
                                  E_Last : Integer;
                                  Ok : out Boolean) do
                        declare
                            Cnt : Natural;
                        begin
                            if Poe.D3 = D3 then
                                -- Can't think of a better way to check the
                                -- bounds of the entry family.
                                begin
                                    Cnt := E (E_First)'Count;
                                    Cnt := E (E_Last)'Count;
                                exception
                                    when Constraint_Error =>
                                        Ok := False;
                                        return;
                                end;
                                begin
                                    Cnt := E (E_First - 1)'Count;
                                    Ok := False;
                                    return;
                                exception
                                    when Constraint_Error =>
                                        null;
                                    when others =>
                                        Ok := False;
                                        return;
                                end;
                                begin
                                    Cnt := E (E_Last + 1)'Count;
                                    Ok := False;
                                    return;
                                exception
                                    when Constraint_Error =>
                                        null;
                                    when others =>
                                        Ok := False;
                                        return;
                                end;
                                Ok := True;
                            else
                                Ok := False;
                                return;
                            end if;
                        end;
                    end Is_Ok;
                or
                    terminate;
                end select;
            end loop;
        end Poe;

        function Is_Ok
                    (C : Poe; D3 : Integer; E_First : Integer; E_Last : Integer)
                    return Boolean is
            Ok : Boolean;
        begin
            C.Is_Ok (D3, E_First, E_Last, Ok);
            return Ok;
        end Is_Ok;

        procedure Chk is new Check (Poe, Is_Ok);

    begin
        Chk;
    end;


    Comment ("Discriminant-dependent entry families for protected types");

    F1_Poe := 18;

    declare
        protected type Poe (D3 : Integer := F1) is
            entry E (D3 .. F1);    -- F1 evaluated
            function Is_Ok (D3 : Integer; E_First : Integer; E_Last : Integer)
                           return Boolean;
        end Poe;
        protected body Poe is
            entry E (for I in D3 .. F1) when True is
            begin
                null;
            end E;
            function Is_Ok (D3 : Integer; E_First : Integer; E_Last : Integer)
                           return Boolean is
                Cnt : Natural;
            begin
                if Poe.D3 = D3 then
                    -- Can't think of a better way to check the
                    -- bounds of the entry family.
                    begin
                        Cnt := E (E_First)'Count;
                        Cnt := E (E_Last)'Count;
                    exception
                        when Constraint_Error =>
                            return False;
                    end;
                    begin
                        Cnt := E (E_First - 1)'Count;
                        return False;
                    exception
                        when Constraint_Error =>
                            null;
                        when others =>
                            return False;
                    end;
                    begin
                        Cnt := E (E_Last + 1)'Count;
                        return False;
                    exception
                        when Constraint_Error =>
                            null;
                        when others =>
                            return False;
                    end;
                    return True;
                else
                    return False;
                end if;
            end Is_Ok;
        end Poe;

        function Is_Ok
                    (C : Poe; D3 : Integer; E_First : Integer; E_Last : Integer)
                    return Boolean is
        begin
            return C.Is_Ok (D3, E_First, E_Last);
        end Is_Ok;

        procedure Chk is new Check (Poe, Is_Ok);

    begin
        Chk;
    end;

    Comment ("Protected components");

    F1_Poe := 18;

    declare
        protected type Poe (D3 : Integer := F1) is
            function C1_D1 return Integer;
            function C1_D2 return Integer;
        private
            C1 : Rec (D3, F1);    -- F1 evaluated
        end Poe;
        protected body Poe is
            function C1_D1 return Integer is
            begin
                return C1.D1;
            end C1_D1;
            function C1_D2 return Integer is
            begin
                return C1.D2;
            end C1_D2;
        end Poe;

        function Is_Ok (C : Poe; D3 : Integer; C1_D1 : Integer; C1_D2 : Integer)
                       return Boolean is
        begin
            return C.D3 = D3 and C.C1_D1 = C1_D1 and C.C1_D2 = C1_D2;
        end Is_Ok;

        procedure Chk is new Check (Poe, Is_Ok);

    begin
        Chk;
    end;

    Result;

exception
    when others =>
        Failed ("Unexpected exception");
        Result;

end C380004;
