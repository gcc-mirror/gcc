! { dg-do  run }
! { dg-additional-options "-fno-frontend-optimize -fdump-tree-original" }
program main
  integer, parameter :: A(3,2) = reshape([1,2,3,4,5,6],[3,2])
  integer, parameter :: B(2,3) = reshape([1,1,1,1,1,1],[2,3])
  character (len=30) :: line
  write (unit=line,fmt='(9i3)') matmul(A,B)
  if (line /= '  5  7  9  5  7  9  5  7  9') STOP 1
end program main
! { dg-final { scan-tree-dump-times "matmul_i4" 0 "original" } }
