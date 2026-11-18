! { dg-do run }

program p
  implicit none
  integer :: a(10), b(10,10), k(2) = [1,3]
  real, allocatable :: r(:,:)
  character(len=4), target :: c(10,10)
  allocate (r(4,4))
  if (is_contiguous (a([1,3]))) stop 1
  if (is_contiguous (a(k))) stop 2
  if (is_contiguous (b(:,[1,3]))) stop 3
  if (is_contiguous (r(:,k))) stop 4
  call s (b)
  call t (c)
contains
  subroutine s (x)
    integer :: x(:,:)
    if (is_contiguous (x(:,[1,3]))) stop 5
    if (is_contiguous (x([1,3],1))) stop 6
    if (is_contiguous (x([1,2],:))) stop 7
  end subroutine
  subroutine t (y)
    character(len=4), target :: y(:,:)
    if (is_contiguous (y(:,[1,3]))) stop 8
    if (is_contiguous (y([1,2],:)(1:4))) stop 9
    if (is_contiguous (y(:,[1,2])(1:2))) stop 10
  end subroutine
end program
