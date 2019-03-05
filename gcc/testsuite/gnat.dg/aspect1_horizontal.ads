with Aspect1_Vectors_2D;

generic
   with package Position_2d_Pkg is new Aspect1_Vectors_2D (<>);
   with package Speed_2d_Pkg is new Aspect1_Vectors_2D (<>);
package Aspect1_Horizontal is
   function Theta_D(s: Position_2d_Pkg.Vect2; nzv: Speed_2d_Pkg.Nz_vect2)
      return float;
end Aspect1_Horizontal;
