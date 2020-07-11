package body Array39_Pkg is

  task Body Tsk is
  begin
    select
      accept E (R : out Rec2; L : Index2) do
      declare
        A  : Arr2 (Index2);
        LL : Index2 := L;
      begin
        for I in 1 .. LL loop
          A (I) := Val;
        end loop;
        R := (D => LL, A => A (1 .. LL));
      end;
      end E;
    end select;
  end Tsk;

end Array39_Pkg;
