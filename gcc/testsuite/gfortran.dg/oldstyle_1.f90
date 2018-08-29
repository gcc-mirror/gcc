! { dg-do run }
      integer i, j /1/, g/2/, h ! { dg-warning "" }
      integer k, l(3) /2*2,1/   ! { dg-warning "" }
      real pi /3.1416/, e       ! { dg-warning "" }

      if (j /= 1) STOP 1
      if (g /= 2) STOP 2
      if (any(l /= (/2,2,1/))) STOP 3
      if (pi /= 3.1416) STOP 4
      end
