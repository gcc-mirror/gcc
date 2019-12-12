with ada.numerics.generic_elementary_functions;
with Dimensions2_real_numbers;

package Dimensions2_Phys is

   type si_type is new Dimensions2_real_numbers.Real with
      dimension_system =>
      ((unit_name => meter, unit_symbol => 'm', dim_symbol => 'L'),
       (unit_name => kilogram, unit_symbol => "kg", dim_symbol => 'M'),
       (unit_name => second, unit_symbol => 's', dim_symbol => 'T'),
       (unit_name => ampere, unit_symbol => 'A', dim_symbol => 'I'),
       (unit_name => kelvin, unit_symbol => 'K', dim_symbol => "Theta"),
       (unit_name => mole, unit_symbol => "mol", dim_symbol => 'N'),
       (unit_name => euro, unit_symbol => "EUR", dim_symbol => 'E'));

   subtype distance is Si_Type with
        dimension => (symbol => 'm', meter => 1, others => 0);

   subtype mass is Si_Type with
        dimension => (symbol => "kg", kilogram => 1, others => 0);

   subtype time is Si_Type with
        dimension => (symbol => 's', second => 1, others => 0);

   subtype electric_current is Si_Type with
        dimension => (symbol => 'A', ampere => 1, others => 0);

   subtype temperature is Si_Type with
        dimension => (symbol => 'K', kelvin => 1, others => 0);

   subtype amount_of_substance is Si_Type with
        dimension => (symbol => "mol", mole => 1, others => 0);

   pragma warnings (off, "*assumed to be*");
   subtype pressure_barg is Dimensions2_real_numbers.Real;
   m : constant Distance := 1.0;
   kg : constant Mass := 1.0;
   s : constant Time := 1.0;
   a : constant Electric_Current := 1.0;
   k : constant Temperature := 1.0;
   mol : constant Amount_Of_Substance := 1.0;
   min : constant Time := 1.0;
   h : constant Time := 60.0 * min;

   subtype frequency is Si_Type with
        dimension => (symbol => "Hz", second => -1, others => 0);

   subtype massflow is Si_Type with
     dimension => (symbol => "kg/s",
       kilogram => 1, second => -1, others => 0);

   subtype molar_heat_capacity is Si_Type with
     dimension => (symbol => "J/(K*mol)", meter => 2, kilogram => 1,
       second => -2, kelvin => -1, mole => -1, others => 0);

   subtype molar_flow is Si_Type with
      dimension => (symbol => "mol/s", second => -1, mole => 1, others => 0);

   subtype pressure is Si_Type with
    dimension =>
     (symbol => "Pa", meter => -1, kilogram => 1, second => -2, others => 0);

   subtype ratio is Si_Type range 0.0 .. 1.0;

   subtype scalar is Si_Type;

   subtype specific_heat_capacity is Si_Type with
     dimension => (symbol => "J/(K*kg)", meter => 2, second => -2,
       kelvin => -1, others => 0);

   subtype speed is Si_Type with
     dimension => (symbol => "m/s", meter => 1, second => -1, others => 0);

   subtype volume is Si_Type with
        dimension => (symbol => "m^3", meter => 3, others => 0);

   subtype volumetric_flow is Si_Type with
     dimension => (symbol => "m^3/s", meter => 3, second => -1, others => 0);

end Dimensions2_Phys;
