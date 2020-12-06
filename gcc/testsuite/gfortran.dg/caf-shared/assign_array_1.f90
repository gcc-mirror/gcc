! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "2" }
program main
  implicit none
  integer, dimension(5):: a[*]
  a(1) = 1 + 100*this_image()
  a(2) = 2
  a(3) = 3
  a(4) = 4
  a(5) = 5
  sync all
  call foo(a)
contains
  subroutine foo(z) 
    integer, dimension(:), intent(in):: z[*]
    if (any(a /= [1+100*this_image(), 2,3,4,5])) stop 1
  end subroutine
end program
