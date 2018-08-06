--  { dg-do compile }

procedure Equal3 is
    type R is record
       A, B : Integer;
    end record;

    package Pack is
       type RR is record
          C : R;
       end record;

       X : RR := (C => (A => 1, B => 1));
       Y : RR := (C => (A => 1, B => 2));
       pragma Assert (X /= Y); --@ASSERT:PASS

    end Pack;
    use Pack;
    function "=" (X, Y : R) return Boolean is (X.A = Y.A); --  { dg-error "equality operator must be declared before type \"R\" is frozen \\(RM 4.5.2 \\(9.8\\)\\) \\(Ada 2012\\)" }
begin
    pragma Assert (X /= Y); --@ASSERT:FAIL
end Equal3;
