c { dg-do run }
      integer(kind=1) i1, i11
      integer(kind=2) i2, i22
      integer         i, ii
      integer(kind=4) i4, i44
      integer(kind=8) i8, i88
      real      r, rr
      real(kind=4)    r4, r44
      double precision d, dd
      real(kind=8)   r8, r88
      parameter (i1 = 1, i2 = 2, i4 = 4, i = 5, i8 = i + i4*i2 + i2*i1)
      parameter (r = 3.0, r4 = 4.0, r8 = 8.d0, d = i8*r + r4*i2 + r8*i1)
      if (i8 .ne. 15   ) call abort
      if (d  .ne. 61.d0) call abort
      i11 = 1; i22 = 2; i44 = 4; ii = 5
      i88 = i + i4*i2 + i2*i1
      if (i88 .ne. i8) call abort
      rr = 3.0; r44 = 4.0; r88 = 8.0d0
      dd = i88*rr + r44*i22 + r88*i11
      if (dd .ne. d) call abort
      end
