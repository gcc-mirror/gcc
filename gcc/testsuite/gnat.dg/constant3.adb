-- { dg-do compile }
-- { dg-options "-O" }

with System.Machine_code; use System.Machine_code;

procedure Constant3 is

  c : Integer := -1;
  r : Integer;

  procedure Conv (res : out Integer; v : Integer) is
    v1 : constant Integer := v;
  begin
    Asm ("", Integer'Asm_output ("=m", res), Integer'Asm_input("m", v1));
  end;

  pragma Inline_Always (Conv);

begin
  Conv (r, c);
end;
