package body Tail_Call_P is

  function Start_Side (Element : T) return Index is
  begin
    if Element = 1 then
      raise Program_Error;
    end if;
    if Element = 0 then
      return Second;
    else
      return First;
    end if;
  end;

  function Segment (Element : T) return T is
  begin
    if Element /= 0 then
      raise Program_Error;
    end if;
    return 1;
  end;

  procedure Really_Insert (Into : T; Element : T; Value : T) is
  begin
    if Into /= 0 then
      raise Program_Error;
    end if;
  end;

  procedure Insert (Into : A; Element : T; Value : T) is
  begin
    Really_Insert (Into (Start_Side (Element)), Segment (Element), Value);
  end Insert;

end Tail_Call_P;
