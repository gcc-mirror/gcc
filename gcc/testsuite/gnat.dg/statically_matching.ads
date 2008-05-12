package Statically_Matching is
   type T1(b: boolean) is tagged null record;
   type T2 is new T1(b => false) with private;
private
   F: constant boolean := false;
   type T2 is new T1(b => F) with null record;  -- OK
end Statically_Matching;
