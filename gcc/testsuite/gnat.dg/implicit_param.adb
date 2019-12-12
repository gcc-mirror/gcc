--  { dg-do compile }

with Implicit_Param_Pkg;

procedure Implicit_Param is
    subtype Tiny is Integer range 1 .. 5;
    V : Tiny := 4;

    function Func62 return Implicit_Param_Pkg.Lim_Rec is
    begin
       return
         (case V is
           when 1 .. 3 => Implicit_Param_Pkg.Func_Lim_Rec,
           when 4 .. 5 => raise Program_Error);
    end Func62;

begin
    null;
end Implicit_Param;
