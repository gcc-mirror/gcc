package Array18_Pkg is

   function N return Positive;

   subtype S is String (1 .. N);

   function F return S;

end Array18_Pkg;
