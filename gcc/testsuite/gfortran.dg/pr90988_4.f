c { dg-do compile }
       module foo
          implicit none
          real a,b,c
          integer i,j,k
          public a,b
          publicc
          private i,j
          privatek
       end module foo
