-- { dg-do compile }
-- { dg-options "-Wstack-usage=512" }

procedure Stack_Usage5 (C : Character) is

  S : String (1 .. 300);

  procedure Set is
  begin
    S (1) := C;
  end;

begin
  Set;
end;
