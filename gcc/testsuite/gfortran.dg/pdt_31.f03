! { dg-do run }
!
! Test the fix for PR100110, in which 'obj' was not being initialized.
!
! Contributed by Xiao Liu  <xiao.liu@compiler-dev.com>
!
program p
  implicit none
  type t(n)
    integer, len :: n
    integer :: arr(n, n)
  end type

  type(t(2)) :: obj

  obj%arr = reshape ([1,2,3,4],[2,2])
  if (obj%n .ne. 2) stop 1
  if (any (shape(obj%arr) .ne. [2,2])) stop 2
  call test()
contains
  subroutine test()
    if (obj%n .ne. 2) stop 3
    if (any (shape(obj%arr) .ne. [2,2])) stop 4
    if (any (reshape (obj%arr, [4]) .ne. [1,2,3,4])) stop 5
  end subroutine
end program
