package Implicit_Param_Pkg is
    type Lim_Rec is limited record
        A : Integer;
        B : Boolean;
    end record;

    function Func_Lim_Rec return Lim_Rec;
end Implicit_Param_Pkg;
