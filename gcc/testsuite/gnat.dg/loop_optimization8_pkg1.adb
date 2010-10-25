with Loop_Optimization8_Pkg2;

package body Loop_Optimization8_Pkg1 is

  Data : Loop_Optimization8_Pkg2.T
    := new Loop_Optimization8_Pkg2.Obj_T'(Length =>1, Elements => (1 => 1));

  procedure Iter is
  begin
    for I in 1 .. Loop_Optimization8_Pkg2.Length (Data) loop
      Action (Loop_Optimization8_Pkg2.Index (Data, I));
    end loop;
  end;

end Loop_Optimization8_Pkg1;
