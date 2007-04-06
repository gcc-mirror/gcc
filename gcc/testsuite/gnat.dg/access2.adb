-- { dg-do compile }

procedure access2 is
  Arr : array (1..10) of aliased Float;
  type Acc is access all Float;
  procedure Set (X : integer) is
     Buffer: String (1..8);
     for Buffer'address use Arr (4)'address;
  begin
     Arr (X) := 31.1415;
  end;
  function Get (C : Integer) return Acc is
  begin
     return Arr (C)'access;
  end;
begin
   null;
end;
