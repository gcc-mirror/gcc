--  { dg-do run }
--  { dg-options -gnata }

with Prot5_Pkg;

procedure Prot5 is
begin
   Prot5_Pkg.P.Proc (10);                   --  explicit parameter
   Prot5_Pkg.P.Proc (Prot5_Pkg.P.Get_Data); --  explicit call to protected operation
   Prot5_Pkg.P.Proc;                        -- defaulted call.
   pragma Assert (Prot5_Pkg.P.Get_Data = 80);
end Prot5;
