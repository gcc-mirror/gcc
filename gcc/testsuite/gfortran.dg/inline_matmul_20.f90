! { dg-do  run }
! { dg-additional-options "-fno-realloc-lhs -ffrontend-optimize" }
! This used to segfault at runtime.
! Original test case by Harald Anlauf.
program gfcbug142
  implicit none
  real, allocatable :: b(:,:)
  integer :: n = 5
  character(len=20) :: line
  allocate (b(n,n))
  call random_number (b)
  write (unit=line,fmt='(2I5)') shape (matmul (b, transpose (b)))
  if (line /= '    5    5') STOP 1
end program gfcbug142
