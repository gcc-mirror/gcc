-- { dg-do compile }
-- { dg-options "-O" }

with System, Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

procedure Loop_Address2 is

  type Ptr is access all Integer;

  function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Ptr);

  function F (BM : System.Address; I : Integer) return System.Address is
  begin
    return BM + Storage_Offset (4*I);
  end;

  B : Integer;
  P : Ptr;

begin
  for I in 0 .. 2 loop
    P := To_Ptr (F (B'Address, I));
    P.all := 0;
  end loop;
end ;
