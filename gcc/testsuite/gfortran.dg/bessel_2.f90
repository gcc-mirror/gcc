! { dg-do compile }
! PR fortran/36117
!
! This program will fail for MPFR < 2.3.0
!
! Based on a test by James Van Buskirk.
!
program bug3
   implicit none
   real, parameter :: Qarg1 = 1.7
   integer, parameter :: k2 = kind(BESJ0(Qarg1))
   integer, parameter :: is_int = 1-1/(2+0*BESJ0(Qarg1))*2
   integer, parameter :: kind_if_real = &
      (1-is_int)*k2+is_int*kind(1.0)
   complex :: z = cmplx(0,1,kind_if_real) ! FAILS
   if (kind_if_real /= kind(Qarg1)) call abort ()
end program bug3
