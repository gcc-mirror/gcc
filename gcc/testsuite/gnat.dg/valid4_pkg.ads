package Valid4_Pkg is
   Global : Boolean := False;

   procedure Proc (B : in out Boolean);
   pragma Export_Procedure
     (Proc,
      External        => "Proc",
      Parameter_Types => (Boolean),
      Mechanism       => Reference);
end Valid4_Pkg;
