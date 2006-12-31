C { dg-do run }
C { dg-additional-sources c_by_val.c }
C { dg-options "-ff2c -w -O0" }

      program c_by_val_1
      external   f_to_f, i_to_i, c_to_c
      real       a, b, c
      integer*4  i, j, k
      complex    u, v, w, c_to_c

      a = 42.0
      b = 0.0
      c = a
      call  f_to_f (b, %VAL (a), %REF (c), %LOC (c))
      if ((2.0 * a).ne.b) call abort ()

      i = 99
      j = 0
      k = i
      call i_to_i (j, %VAL (i), %REF (k), %LOC (k))
      if ((3 * i).ne.j) call abort ()

      u = (-1.0, 2.0)
      v = (1.0, -2.0)
      w = u
      v = c_to_c (%VAL (u), %REF (w), %LOC (w))
      if ((4.0 * u).ne.v) call abort ()

      stop
      end

