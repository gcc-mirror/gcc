! { dg-do compile }
! { dg-options "-Wall" }
! Code contributed by Manfred Schwarb <manfred99 at gmx dot ch>
! PR fortran/91497
!
! Prior to applying the patch for this PR, the following code
! would generate numerous conversion warnings.
! Additional test case to cover all targets.
!
program foo

      real(4) a, aa
      real(8) b, bb
      integer(2) e, ee
      integer(4) f, ff
      integer(8) g, gg
      complex(4) ww
      complex(8) xx
      PARAMETER(a=3.1415927_4)
      PARAMETER(b=3.1415927_8)
      PARAMETER(e=123_2)
      PARAMETER(f=123_4)
      PARAMETER(g=123_8)

      aa=REAL(b)    ! was: Change of value in conversion from 'REAL(8)' to 'REAL(4)'
      aa=REAL(e)
      aa=REAL(f)
      aa=REAL(g)
      aa=REAL(b, kind=4)   ! was: Change of value in conversion from 'REAL(8)' to 'REAL(4)'
      bb=REAL(a, kind=8)

      aa=FLOAT(f)
      bb=DFLOAT(g)
      aa=SNGL(b)    ! was: Change of value in conversion from 'REAL(8)' to 'REAL(4)'
      aa=AINT(a)
      bb=AINT(b)
      aa=AINT(b, kind=4)
      bb=DINT(b)
      aa=ANINT(a)
      bb=ANINT(b)
      aa=ANINT(b, kind=4)
      bb=DNINT(b)
      aa=AMAX0(f, f)
      aa=AMIN0(f, f)
      aa=AMAX0(g, g)
      aa=AMIN0(g, g)

      ee=INT(a)
      ee=INT(a, kind=2)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(2)'
      ee=INT(b, kind=2)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(2)'
      ee=INT(f, kind=2)
      ee=INT(g, kind=2)
      ff=INT(b)
      ff=INT(a, kind=4)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(4)'
      ff=INT(b, kind=4)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(4)'
      ff=INT(f, kind=4)
      ff=INT(g, kind=4)
      gg=INT(a)
      gg=INT(a, kind=8)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(8)'
      gg=INT(b, kind=8)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(8)'
      gg=INT(f, kind=8)
      gg=INT(g, kind=8)

      ee=IFIX(a)
      ff=IFIX(a)
      gg=IFIX(a)
      ee=IDINT(b)
      ff=IDINT(b)
      gg=IDINT(b)
      ee=INT2(a)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(2)'
      ee=INT2(b)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(2)'
      ee=INT2(f)
      ee=INT2(g)
      gg=INT8(a)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(8)'
      gg=INT8(b)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(8)'
      gg=INT8(f)
      gg=INT8(g)

      ff=FLOOR(b)
      ee=FLOOR(b, kind=2)
      ff=FLOOR(b, kind=4)
      gg=FLOOR(b, kind=8)
      ff=CEILING(b)
      ee=CEILING(b, kind=2)
      ff=CEILING(b, kind=4)
      gg=CEILING(b, kind=8)
      ff=MAX1(a, a)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(4)'
      ff=MIN1(a, a)    ! was: Change of value in conversion from 'REAL(4)' to 'INTEGER(4)'
      gg=MAX1(b, b)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(4)'
      gg=MIN1(b, b)    ! was: Change of value in conversion from 'REAL(8)' to 'INTEGER(4)'

      ee=NINT(a, kind=2)
      ee=NINT(b, kind=2)
      ff=NINT(a)
      ff=NINT(b)
      ff=NINT(a, kind=4)
      ff=NINT(b, kind=4)
      gg=NINT(a, kind=8)
      gg=NINT(b, kind=8)
      ee=IDNINT(b)
      ff=IDNINT(b)
      gg=IDNINT(b)

      ww=COMPLEX(a, a)
      ww=COMPLEX(e, e)
      ww=COMPLEX(g, g)
      ww=COMPLEX(a, g)
      xx=COMPLEX(b, g)
      ww=CMPLX(a, a)
      ww=CMPLX(b, b, kind=4)
      xx=CMPLX(a, a, kind=8)

      aa=REAL(ww)
      bb=REAL(xx)
      aa=REALPART(ww)
      bb=REALPART(xx)
      aa=AIMAG(ww)
      bb=AIMAG(xx)
      aa=IMAG(ww)
      bb=IMAG(xx)
      bb=DIMAG(xx)
      aa=IMAGPART(ww)
      bb=IMAGPART(xx)
end
