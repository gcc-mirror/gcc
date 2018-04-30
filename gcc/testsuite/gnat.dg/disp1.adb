-- { dg-do run }

with Disp1_Pkg; use Disp1_Pkg;

procedure Disp1 is
   O   : DT_I1;
   Ptr : access I1'Class;
begin
   Ptr := new I1'Class'(I1'Class (O));
end;
