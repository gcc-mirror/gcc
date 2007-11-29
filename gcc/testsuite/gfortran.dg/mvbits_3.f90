! { dg-do run }
!
! PR fortran/
!
! The trans-*.c part of the compiler did no know
! that mvbits is an elemental function.
!
! Test case contributed by P.H. Lundow.
!
program main
  implicit none
  integer :: a( 2 ), b( 2 )
  integer :: x, y

  a = 1
  b = 0
  x = 1
  y = 0

  call mvbits (a, 0, 1, b, 1)
  call mvbits (x, 0, 1, y, 1)

!  write (*, *) 'a: ', a
!  write (*, *) 'x: ', x
!  write (*, *)
!  write (*, *) 'b: ', b
!  write (*, *) 'y: ', y
!  write (*, *)

  if ( any (b /= y) ) call abort()
end program main
