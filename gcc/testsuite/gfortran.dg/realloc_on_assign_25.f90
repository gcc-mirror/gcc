! { dg-do run }
! PR 47674 - this would segfault if MALLOC_PERTURB is set.
! This checks a code path where it is not possible to determine
! the length of the string at compile time.
!
program main
  implicit none
  character(:), allocatable :: a
  integer :: m, n
  a = 'a'
  if (a .ne. 'a') call abort
  a = a // 'x'
  if (a .ne. 'ax') call abort
  if (len (a) .ne. 2) call abort
  n = 2
  m = 2
  a = a(m:n)
  if (a .ne. 'x') call abort
  if (len (a) .ne. 1) call abort
end program main
