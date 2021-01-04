! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! This test only works with four images, it will fail otherwise.
program main
  implicit none
  integer, parameter :: n = 3
  integer, dimension(n) :: a
  a = [1,2,3] + this_image()
  call co_reduce (a, mysum, result_image = 2)
  if (this_image () == 2) then
     if (any(a /= [14,18,22])) then
        print *,a
        print *,a /= [14,18,22]
        print *,any(a /= [14,18,22])
        stop 1
     end if
  end if
contains
  PURE FUNCTION mysum (lhs,rhs)
    integer, intent(in) :: lhs, rhs
    integer :: mysum
    mysum = lhs + rhs
  END FUNCTION mysum
end program main
