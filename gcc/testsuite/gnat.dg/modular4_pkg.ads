package Modular4_Pkg is

   type Word is mod 2**48;

   Zero : constant Word := 0;

   function F return Word;

end Modular4_Pkg;
