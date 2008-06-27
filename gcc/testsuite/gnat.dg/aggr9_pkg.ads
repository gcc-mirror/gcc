package Aggr9_Pkg is

  type Byte is range 0 .. 255;

  type R1 is
    record
      A,B : Byte;                    
    end record;

  type R2 is
    record
      F : R1;
    end record;

  procedure Send (M : R2);

end Aggr9_Pkg;
