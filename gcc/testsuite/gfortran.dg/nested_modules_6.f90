! { dg-do compile }
! Test the patch for PR30084 in which the reference to SIZE
! in function diag caused a segfault in module.c.
!
! Contributed by Troban Trumsko <trumsko@yahoo.com>
! and reduced by Steve Kargl <kargl@gcc.gnu.org>
!
module tao_random_numbers
  integer, dimension(10) :: s_buffer
  integer :: s_last = size (s_buffer)
end module tao_random_numbers

module linalg
  contains
  function diag (a) result (d)
    real, dimension(:,:), intent(in) :: a
    real, dimension(min(size(a,dim=1),size(a,dim=2))) :: d
    integer :: i
    do i = 1, min(size(a, dim = 1), size(a, dim = 2))
       d(i) = a(i,i)
    end do
  end function diag
end module linalg

module vamp_rest
  use tao_random_numbers
  use linalg
end module vamp_rest

  use vamp_rest
  real :: x(2, 2) = reshape ([1.,2.,3.,4.], [2,2])
  print *, s_last
  print *, diag (x)
end
! { dg-final { cleanup-modules "tao_random_numbers linalg vamp_rest" } }
