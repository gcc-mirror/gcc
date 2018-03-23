! { dg-do run }
! Test the fix for PR38802, in which the nulling of the result 'p'
! in 'a_fun' would cause a segfault.
!
! Posted on the gfortran list by Marco Restelli http://gcc.gnu.org/ml/fortran/2009-01/

!
module mod_a
  implicit none
  public :: a_fun, t_1, t_2
  private
  type t_1
    real :: coeff
  end type t_1
  type t_2
    type(t_1), allocatable :: mons(:)
  end type t_2
contains
  function a_fun(r) result(p)
    integer, intent(in) :: r
    type(t_2) :: p(r+1)
    p = t_2 ([t_1 (99)])
  end function a_fun
end module mod_a

program test
  use mod_a, only: a_fun, t_1, t_2
  implicit none
  type(t_2) x(1)
  x = a_fun(0)
  if (any (x(1)%mons%coeff .ne. 99)) STOP 1
end program test
