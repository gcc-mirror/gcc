C { dg-do run }
C { dg-additional-sources c_by_val.c }
C { dg-options "-ff2c -w -O0" }

      program c_by_val_1
      external   f_to_f, i_to_i, c_to_c
      external   f_to_f8, i_to_i8, c_to_c8
      real       a, b, c
      real(8)    a8, b8, c8
      integer(4) i, j, k
      integer(8) i8, j8, k8
      complex    u, v, w
      complex(8) u8, v8, w8

      a = 42.0
      b = 0.0
      c = a
      call  f_to_f (b, %VAL (a), %REF (c), %LOC (c))
      if ((2.0 * a).ne.b) STOP 1

      a8 = 43.0
      b8 = 1.0
      c8 = a8
      call  f_to_f8 (b8, %VAL (a8), %REF (c8), %LOC (c8))
      if ((2.0 * a8).ne.b8) STOP 2

      i = 99
      j = 0
      k = i
      call i_to_i (j, %VAL (i), %REF (k), %LOC (k))
      if ((3 * i).ne.j) STOP 3

      i8 = 199
      j8 = 10
      k8 = i8
      call i_to_i8 (j8, %VAL (i8), %REF (k8), %LOC (k8))
      if ((3 * i8).ne.j8) STOP 4

      u = (-1.0, 2.0)
      v = (1.0, -2.0)
      w = u
      call c_to_c (v, %VAL (u), %REF (w), %LOC (w))
      if ((4.0 * u).ne.v) STOP 5

      u8 = (-1.0, 2.0)
      v8 = (1.0, -2.0)
      w8 = u8
      call c_to_c8 (v8, %VAL (u8), %REF (w8), %LOC (w8))
      if ((4.0 * u8).ne.v8) STOP 6

      stop
      end

