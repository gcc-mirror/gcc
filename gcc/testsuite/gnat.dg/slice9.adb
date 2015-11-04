-- { dg-do compile }

procedure Slice9 is

  function Ident (I : Integer) return Integer is
  begin
    return I;
  end;

  subtype S is String (Ident(5)..Ident(9));

  Dest : S;

  Src : String (Ident(1)..Ident(5)) := "ABCDE";

begin
  Dest (Ident(5)..Ident(7)) := Src (Ident(1)..Ident(3));
end;
