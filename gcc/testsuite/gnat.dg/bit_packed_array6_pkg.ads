package Bit_Packed_Array6_Pkg is

   type Project_Kind is
     (K_Configuration, K_Abstract,
      K_Standard, K_Library, K_Aggregate, K_Aggregate_Library);

   type Projects_Kind is array (Project_Kind) of Boolean
     with Pack,
          Dynamic_Predicate => Projects_Kind /= (Project_Kind => False);

   Everywhere : constant Projects_Kind := (others => True);

end Bit_Packed_Array6_Pkg;
