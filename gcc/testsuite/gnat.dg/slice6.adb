-- { dg-do compile }
-- { dg-options "-gnatws" }

with Slice6_Pkg; use Slice6_Pkg;

procedure Slice6 is

  procedure Send (V_LENGTH : SHORT_INTEGER) is

    V : Integer;

    V_BLOCK : T_BLOCK (1 .. 4096);
    for V_BLOCK use at V'Address;

    V_MSG : T_MSG ;

  begin
    V_MSG := (V_LENGTH, 1, V_BLOCK (1 .. V_LENGTH));
  end;

begin
  null;
end;
