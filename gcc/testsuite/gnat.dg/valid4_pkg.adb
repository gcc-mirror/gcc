package body Valid4_Pkg is
   procedure Inner_Proc (B : in out Boolean);
   pragma Export_Procedure
     (Inner_Proc,
      External        => "Inner_Proc",
      Parameter_Types => (Boolean),
      Mechanism       => Reference);

   procedure Inner_Proc (B : in out Boolean) is
   begin
      B := True;
      Global := False;
   end Inner_Proc;

   procedure Proc (B : in out Boolean) is
   begin
      Inner_Proc (B);
   end Proc;
end Valid4_Pkg;
