! { dg-do compile }
! { dg-require-effective-target fortran_real_10 }
! { dg-require-effective-target fortran_real_16 }
! { dg-options "-Wall" }
! Code contributed by Manfred Schwarb <manfred99 at gmx dot ch>
! PR fortran/91497
!
! Prior to applying the patch for this PR, the following code
! would generate numerous conversion warnings.
!
program foo

      real(4) a,aa
      real(8) b,bb
      real(10) c,cc
      real(16) d
      integer(2) e,ee
      integer(4) f,ff
      integer(8) g,gg
      PARAMETER(a=3.1415927_4)
      PARAMETER(b=3.1415927_8)
      PARAMETER(c=3.1415927_10)
      PARAMETER(d=3.1415927_16)
      PARAMETER(e=123_2)
      PARAMETER(f=123_4)
      PARAMETER(g=123_8)

      aa=REAL(b)
      aa=REAL(c)
      aa=REAL(d)
      aa=REAL(e)
      aa=REAL(f)
      aa=REAL(g)
      aa=FLOAT(f)
      aa=FLOOR(b)
      aa=FLOOR(c)
      aa=FLOOR(d)
      aa=CEILING(b)
      aa=CEILING(c)
      aa=CEILING(d)
      !---DEC specific type conversions (-fdec):
      !!aa=FLOATI(e)
      !!aa=FLOATJ(f)
      !!aa=FLOATK(g)
      aa=SNGL(c)
      aa=SNGL(d)
      bb=REAL(c, kind=8)
      bb=REAL(d, kind=8)
      bb=DBLE(c)
      bb=DBLE(d)
      bb=DFLOAT(g)
      bb=FLOOR(c)
      bb=FLOOR(d)
      bb=CEILING(c)
      bb=CEILING(d)
      cc=REAL(d, kind=10)
      cc=FLOOR(d)
      cc=CEILING(d)

      aa=AINT(b)
      aa=ANINT(b)
      aa=AINT(c)
      aa=ANINT(c)
      aa=AINT(d)
      aa=ANINT(d)
      bb=DINT(b)
      bb=DNINT(b)

      ee=INT(a, kind=2)
      ee=NINT(a, kind=2)
      ee=INT(b, kind=2)
      ee=NINT(b, kind=2)
      ee=INT(c, kind=2)
      ee=NINT(c, kind=2)
      ee=INT(d, kind=2)
      ee=NINT(d, kind=2)
      ee=INT(f, kind=2)
      ee=INT(g, kind=2)
      ee=IFIX(a)
      ee=IDINT(b)
      ee=IDNINT(b)
      ee=INT2(a)
      ee=INT2(b)
      ee=INT2(c)
      ee=INT2(d)
      ee=INT2(f)
      ee=INT2(g)

      ff=INT(a, kind=4)
      ff=NINT(a, kind=4)
      ff=INT(b, kind=4)
      ff=NINT(b, kind=4)
      ff=INT(c, kind=4)
      ff=NINT(c, kind=4)
      ff=INT(d, kind=4)
      ff=NINT(d, kind=4)
      ff=INT(f, kind=4)
      ff=INT(g, kind=4)
      ff=IFIX(a)
      ff=IDINT(b)
      ff=IDNINT(b)
      !---LONG support got removed:
      !!ff=LONG(a)
      !!ff=LONG(b)
      !!ff=LONG(c)
      !!ff=LONG(d)
      !!ff=LONG(g)

      gg=INT(a, kind=8)
      gg=NINT(a, kind=8)
      gg=INT(b, kind=8)
      gg=NINT(b, kind=8)
      gg=INT(c, kind=8)
      gg=NINT(c, kind=8)
      gg=INT(d, kind=8)
      gg=NINT(d, kind=8)
      gg=INT(f, kind=8)
      gg=INT(g, kind=8)
      gg=IFIX(a)
      gg=IDINT(b)
      gg=IDNINT(b)
      gg=INT8(a)
      gg=INT8(b)
      gg=INT8(c)
      gg=INT8(d)
      gg=INT8(g)
end

