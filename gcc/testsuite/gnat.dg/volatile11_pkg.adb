package body Volatile11_Pkg is

   procedure Bit_Test(Input : in Integer;
                      Output1 : out Boolean; Output2 : out Boolean;
                      Output3 : out Boolean; Output4 : out Boolean;
                      Output5 : out Boolean; Output6 : out Boolean;
                      Output7 : out Boolean; Output8 : out Boolean)  is
  begin
    Output8 := B;
    Output7 := Input = 7;
    Output6 := Input = 6;
    Output5 := Input = 5;
    Output4 := Input = 4;
    Output3 := Input = 3;
    Output2 := Input = 2;
    Output1 := Input = 1;
  end Bit_Test;

  function F return Ptr is
  begin
    B := True;
    return B'Access;
  end;

end Volatile11_Pkg;
