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
  if (a .ne. 'a') STOP 1
  a = a // 'x'
  if (a .ne. 'ax') STOP 2
  if (len (a) .ne. 2) STOP 3
  n = 2
  m = 2
  a = a(m:n)
  if (a .ne. 'x') STOP 4
  if (len (a) .ne. 1) STOP 5
end program main
