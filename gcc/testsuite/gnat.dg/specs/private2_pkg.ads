package Private2_Pkg is

   type Rec2 (D : Natural) is private;

private

   type Rec1 (D : Natural) is null record;

   type Rec2 (D : Natural) is new Rec1 (D);

end Private2_Pkg;
