package Unknown_Discr1_Pkg.Child is

  type Derived_1 is new Tagged_Type with null record;

  type Derived_2 is new Derived_1 with null record;

  package Nested is

    type Derived_3 is new Tagged_Type with private;

  private

    type Derived_3 is new Tagged_Type with null record;

  end Nested;

end Unknown_Discr1_Pkg.Child;
