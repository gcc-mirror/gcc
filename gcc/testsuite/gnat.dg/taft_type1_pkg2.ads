package Taft_Type1_Pkg2 is
   type Priv (X : Integer) is private;
private
   type Priv (X : Integer) is null record;
end Taft_Type1_Pkg2;
