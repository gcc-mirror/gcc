! { dg-do compile }
!
! PR fortran/55444
!
! Contributed by Henrik Holst
!
      BLOCKDATA
!       USE ISO_C_BINDING, ONLY: C_INT, C_FLOAT ! WORKS
        USE :: ISO_C_BINDING  ! FAILS
        INTEGER(C_INT) X
        REAL(C_FLOAT) Y
        COMMON /FOO/ X,Y
        BIND(C,NAME='fortranStuff') /FOO/
        DATA X /1/
        DATA Y /2.0/
      END BLOCKDATA
