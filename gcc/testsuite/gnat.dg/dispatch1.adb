-- { dg-do run }

with dispatch1_p; use dispatch1_p;
procedure dispatch1 is
   O   : DT_I1;
   Ptr : access I1'Class;
begin
   Ptr := new I1'Class'(I1'Class (O));
end;
