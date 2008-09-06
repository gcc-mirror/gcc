! { dg-do compile }
!
! PR fortran/36153
! Contributed by Jonathan Hogg
! 
program test_64
   implicit none

   integer, parameter :: long = selected_int_kind(18)
   integer, parameter :: short = kind(0)

   integer(long), parameter :: big_sz = huge(0_short)+1000_long
   integer(long), parameter :: max_32 = huge(0_short)
   integer, dimension(:), allocatable :: array

   integer(long) :: i

   print *, "2**31  = ", 2_long**31
   print *, "max_32 = ", max_32
   print *, "big_sz = ", big_sz

! Disabled as it overflows on 32bit systems (at compile time)
! (conversion of integer(8) to integer(4))
!   allocate(array(big_sz))
   print *, "sz = ", size(array)
   print *, "sz = ", size(array, kind=long)
end program
