      integer i, j /1/, g/2/, h ! { dg-warning "" "" }
      integer k, l(3) /2*2,1/   ! { dg-warning "" "" }
      real pi /3.1416/, e       ! { dg-warning "" "" }

      if (j /= 1) call abort ()
      if (g /= 2) call abort ()
      if (any(l /= (/2,2,1/))) call abort ()
      if (pi /= 3.1416) call abort ()
      end
