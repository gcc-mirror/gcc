-- { dg-do compile }

procedure Atomic2 is

  type Big is array (1..4) of Integer;
  type Arr is array (1..10) of Big;
  pragma Atomic_Components (Arr); -- { dg-warning "cannot be guaranteed" }

begin
  null;
end;
