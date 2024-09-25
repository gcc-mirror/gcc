-- { dg-do run }

with Array42_Pkg; use Array42_Pkg;

procedure Array42 is

  procedure Raise_Error_If_False (Test : Boolean; N : Positive) is
  begin
    if not Test then
      raise Program_Error with "Test" & N'Img & " fails";
    end if;
  end;

begin
  Raise_Error_If_False (LT2  ("12", "21"), 1);
  Raise_Error_If_False (LT4  ("1234", "4321"), 2);
  Raise_Error_If_False (LT8  ("12345678", "87654321"), 3);
  Raise_Error_If_False (LT8  ("12345678", "87654321"), 4);
  Raise_Error_If_False (LT16 ("12345678ABCDEFGH", "HGFEDCBA87654321"), 5);

  Raise_Error_If_False (LT5  ("12345", "54321"), 6);
  Raise_Error_If_False (LE5  ("12345", "54321"), 7);
  Raise_Error_If_False (not GT5  ("12345", "54321"), 8);
  Raise_Error_If_False (not GE5  ("12345", "54321"), 9);

  Raise_Error_If_False (LT45  ("1234", "12345"), 10);
  Raise_Error_If_False (not LT54  ("12345", "1234"), 11);
  Raise_Error_If_False (LT54  ("12345", "1235"), 12);

  Raise_Error_If_False (LT ("1234", "12345"), 13);
  Raise_Error_If_False (not LT ("12345", "1234"), 14);
  Raise_Error_If_False (LT ("12345", "1235"), 15);
end;
