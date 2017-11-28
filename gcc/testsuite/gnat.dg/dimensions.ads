package Dimensions is

   type Mks_Int_Type is new Integer
     with
      Dimension_System => (
        (Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
        (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'),
        (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
        (Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),
        (Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => '@'),
        (Unit_Name => Mole,     Unit_Symbol => "mol", Dim_Symbol => 'N'),
        (Unit_Name => Candela,  Unit_Symbol => "cd",  Dim_Symbol => 'J'));

   subtype Int_Length is Mks_Int_Type
     with
      Dimension => (Symbol => 'm',
        Meter  => 1,
        others => 0);

   subtype Int_Speed is Mks_Int_Type
     with
      Dimension => (
        Meter  =>  1,
        Second => -1,
        others =>  0);

   procedure Dummy;

end Dimensions;
