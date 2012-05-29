-- { dg-do run }

with System;

procedure Array21 is

  type Index_T is mod System.Memory_Size;
  type Arr
    is array (Index_T range Index_T'Last/2-3 .. Index_T'Last/2+3) of Integer;
  C : constant Arr := (1, others => 2);

begin
  if C /= (1, 2, 2, 2, 2, 2, 2) then
    raise Program_Error;
  end if;
end;
