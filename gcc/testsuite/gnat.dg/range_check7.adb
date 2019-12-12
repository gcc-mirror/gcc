--  { dg-do compile }
--  { dg-options "-gnatVa" }

procedure Range_Check7 is

  type Short is range -32768 .. 32767;

  type Int is range -2 ** 31 .. 2 ** 31 - 1;

  subtype Nat is Int range 0 .. Int'Last;

  type Ptr is access all Short;

  procedure Proc (P : Ptr) is
    N : constant Nat := Nat (P.all);
  begin
    null;
  end;

begin
  null;
end;
