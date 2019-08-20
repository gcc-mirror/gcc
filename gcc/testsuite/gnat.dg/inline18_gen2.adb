package body Inline18_Gen2 is

  function Func (I : Interval_T) return T is
    pragma Unreferenced (I);
    Dummy : T;
  begin
    return Dummy;
  end;

end Inline18_Gen2;
