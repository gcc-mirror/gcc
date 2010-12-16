package Opt13_Pkg is

    N : Natural := 0;

    type My_Type is private;

    procedure Allocate (T : out My_Type);

private

    type Data;

    type My_Type is access Data;

end Opt13_Pkg;
