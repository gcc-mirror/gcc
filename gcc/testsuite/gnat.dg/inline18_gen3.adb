package body Inline18_Gen3 is

  package body Inner_G is

    function Next (Position : Index_T) return Index_T is
    begin
      return Position;
    end;

  end Inner_G;

end Inline18_Gen3;
