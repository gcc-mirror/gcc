-- { dg-do run }
-- { dg-options "-O2" }

-- The issue which prompted the test is a compilation failure. Might
-- as well verify that the generated code performs as expected.

with opt64_pkg; use opt64_pkg;

procedure opt64 is
  procedure assert (T : boolean) is
  begin
    if not T then
      raise program_error;
    end if;
  end;
begin
  Encode (1);
  assert (last_hash = "1");
  Encode (2);
  assert (last_hash = "2");
  Encode (3);
  assert (last_hash = "3");
  Encode (6);
  assert (last_hash = "?");
end;
