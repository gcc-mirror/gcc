package body Opt102_Pkg is

  function Get (E : Enum; F, M : access Integer) return Integer is
  begin
    case E is
      when One   => return 0;
      when Two   => return F.all;
      when Three => return M.all;
    end case;
  end;

end Opt102_Pkg;
