-- { dg-do run }

with Aggr19_Pkg; use Aggr19_Pkg;

procedure Aggr19 is
  C : Rec5
    := (Ent => (Kind => Two, Node => (L => (D => True, Pos => 1 )), I => 0));
  A : Rec5 := C;
begin
  Proc (A);
  if A /= C then
    raise Program_Error;
  end if;
end;
