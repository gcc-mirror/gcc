! { dg-do compile }
! { dg-options "-Wconversion" }

!
! PR fortran/27866 -improve -Wconversion
!
SUBROUTINE pr27866
  double precision :: d
  real   :: r
  d = 4d99
  r = d                 ! { dg-warning "conversion" }
END SUBROUTINE

SUBROUTINE pr27866c4
  real(kind=4)    :: a
  real(kind=8)    :: b
  integer(kind=1) :: i1
  integer(kind=4) :: i4
  i4 = 2.3              ! { dg-warning "conversion" }
  i1 = 500              ! { dg-error "overflow" }
  a = 2**26-1           ! { dg-warning "Change of value in conversion" }
  b = 1d999             ! { dg-error "overflow" }

  a = i4                ! assignment INTEGER(4) to REAL(4) - no warning
  b = i4                ! assignment INTEGER(4) to REAL(8) - no warning
  i1 = i4               ! { dg-warning "conversion" }
  a = b                 ! { dg-warning "conversion" }
END SUBROUTINE


!
! PR fortran/35003 - spurious warning with -Wconversion
! Contributed by Brian Barnes <bcbarnes AT gmail DOT com>
!
SUBROUTINE pr35003
  IMPLICIT NONE
  integer(8) :: i, n
  n = 1_8

  do i = 1_8,n
  enddo
END SUBROUTINE


!
! PR fortran/42809 - Too much noise with -Wconversion
! Contributed by Harald Anlauf <anlauf AT gmx DOT de>
!
SUBROUTINE pr42809
  implicit none
  integer, parameter :: sp = kind (1.0)
  integer, parameter :: dp = kind (1.d0)
  real(sp)     :: s
  real(dp)     :: d
  complex(dp)  :: z

  s = 0                 ! assignment INTEGER(4) to REAL(4) - no warning
  d = s                 ! assignment REAL((8)) to REAL(4) - no warning
  z = (0, 1)            ! conversion INTEGER(4) to REAL(4),
                        ! assignment COMPLEX(4) to COMPLEX(8) - no warning
END SUBROUTINE
