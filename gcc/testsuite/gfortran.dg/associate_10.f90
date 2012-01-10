! { dg-do compile }
!
! PR fortran/51383
!
! Contributed by kaiserkarl31@yahoo.com
!
! Was failing before at the ref resolution of y1(1)%i.
!
program extend
   type :: a
      integer :: i
   end type a
   type, extends (a) :: b
      integer :: j
   end type b
   type (a) :: x(2)
   type (b) :: y(2)
   associate (x1 => x, y1 => y)
      x1(1)%i = 1
      ! Commenting out the following line will avoid the error
      y1(1)%i = 2
   end associate
end program extend
