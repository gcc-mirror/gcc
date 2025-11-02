-- { dg-do compile }

procedure Use_Type4 is

  package P1 is
    type T is new Integer;
    function "and" (L, R : in Integer) return T;
  end P1;

  package body P1 is
    function "and" (L, R : in Integer) return T is
    begin
      return T (L * R);
    end "and";
  end P1;

  use type P1.T;

  package Renaming renames P1;

  package P2 is
    use Renaming;
  end P2;

  G : P1.T := Integer'(1) and Integer'(2);

begin
  null;
end;
