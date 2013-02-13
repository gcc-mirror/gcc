! { dg-do run }
!
! I/O test for REAL(16)
!
! Contributed by Dominique d'Humieres
!
program test_qp
   use iso_fortran_env, only: real_kinds
   implicit none
   integer, parameter :: QP = real_kinds(ubound(real_kinds,dim=1))
   real(kind=qp) :: a,b(2), c
   integer :: exponent, i
   character(len=180) :: tmp

   ! Run this only with libquadmath; assume that all those systems
   ! have also kind=10.
   if (size (real_kinds) >= 4 .and. qp == 16) then
     i = 3
     if (real_kinds(i) /= 10) stop

     exponent = 4000
     b(:) = huge (1.0_qp)/10.0_qp**exponent
!     print *, 'real(16) big value:      ', b(1)
     write (tmp, *) b
     read (tmp, *) a, c
!     print *, 'same value read again:   ', a, c
!     print *, 'difference: looks OK now ', a-b(1)
     if (abs (a-b(1))/a > epsilon(0.0_qp) &
         .or. abs (c-b(1))/c > epsilon (0.0_qp)) call abort()
   end if
end program test_qp
