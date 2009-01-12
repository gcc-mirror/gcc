-- { dg-do run }
-- { dg-options "-gnatVa" }

with Unchecked_Conversion;

procedure Unchecked_Convert3 is

  type Word is range -(2**15) .. (2**15) - 1;
  type UWord is mod (2**16);

  function To_Word is new unchecked_conversion (UWord, Word);

  function F return UWord is
  begin
    return 65036;
  end;

  W : Word := To_Word(F);

begin
  null;
end;
