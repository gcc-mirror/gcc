! { dg-do run }
!
! Based on coarray_lib_token_4.f90 but checking whether the bounds
! are correctly handled.
!
program test_caf
  implicit none
  integer, allocatable :: A(:)[:]
  integer, save :: B(3)[*]
  integer :: i

  allocate (A(3)[*])
  A = [1, 2, 3 ] 
  B = [9, 7, 4 ]
  call foo (A, A, test=1)
  call foo (A(2:3), B, test=2)
  call foo (B, A, test=3)
contains
  subroutine foo(x, y, test)
    integer :: x(:)[*]
    integer, contiguous :: y(:)[*]
    integer :: test
    integer :: i, j
    call bar (x)
    call expl (y)
    i = lcobound(x, dim=1)
    j = ucobound(x, dim=1)
    if (i /= 1 .or. j /= num_images()) STOP 1
    i = lcobound(y, dim=1)
    j = ucobound(y, dim=1)
    if (i /= 1 .or. j /= num_images()) STOP 2
  end subroutine foo

  subroutine bar(y)
    integer :: y(:)[*]
    integer :: i, j
    i = lcobound(y, dim=1)
    j = ucobound(y, dim=1)
    if (i /= 1 .or. j /= num_images()) STOP 3
  end subroutine bar

  subroutine expl(z)
    integer :: z(*)[*]
    integer :: i, j
    i = lcobound(z, dim=1)
    j = ucobound(z, dim=1)
    if (i /= 1 .or. j /= num_images()) STOP 4
  end subroutine expl
end program test_caf
