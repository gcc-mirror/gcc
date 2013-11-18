package Volatile11_Pkg is

   procedure Bit_Test(Input : in Integer;
                      Output1 : out Boolean; Output2 : out Boolean;
                      Output3 : out Boolean; Output4 : out Boolean;
                      Output5 : out Boolean; Output6 : out Boolean;
                      Output7 : out Boolean; Output8 : out Boolean);

   type Ptr is access all Boolean;

   B : aliased Boolean := False;

   function F return Ptr;

end Volatile11_Pkg;
