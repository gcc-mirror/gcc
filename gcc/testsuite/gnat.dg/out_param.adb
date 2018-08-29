--  { dg-do compile }
--  { dg-options "-gnat83" }

procedure Out_Param
  (Source : in String; Dest : out String; Char_Count : out Natural) is
begin
    --| Logic_Step:
    --|   Copy string Source to string Dest
  Dest       := (others => ' ');
  Char_Count := 0;
  if Source'Length > 0 and then Dest'Length > 0 then
    if Source'Length > Dest'Length then
      Char_Count := Dest'Length;
    else
      Dest (Dest'First .. (Dest'First + Source'Length - 1)) := Source;
      Char_Count                                            := Source'Length;
    end if;
  else
    null;
  end if;
end Out_Param;
