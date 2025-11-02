package Unknown_Discr1_Pkg is

  type Tagged_Type (<>) is tagged limited private;

private

  type Tagged_Type (Kind : Integer) is tagged limited null record;

end Unknown_Discr1_Pkg;
