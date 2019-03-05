package body Opt75_Pkg is

  overriding procedure Adjust (Object : in out T) is
  begin
    if Object.Ref /= Empty_Rec'Access then
      System.Atomic_Counters.Increment (Object.Ref.Counter);
    end if;
  end;

  A : constant Arr := (others => (others => Empty));

end Opt75_Pkg;
