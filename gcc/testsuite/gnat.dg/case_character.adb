-- { dg-do run }

procedure Case_Character is

  function Test (C : Character) return Integer is
  begin
    case C is
      when ASCII.HT | ' ' .. Character'Last => return 1;
      when others => return 0;
    end case;
  end;

begin

  if Test ('A') /= 1 then
    raise Program_Error;
  end if;

end;
