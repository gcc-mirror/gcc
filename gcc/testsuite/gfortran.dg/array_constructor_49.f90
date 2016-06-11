! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 62106 - this used to give wrong results because
! of a bogus extra temporary variable.
! Original test case by Martien Hulsen
program t
  integer :: ndim=2, ndfp=4, i
  character (len=8) :: line
  write (unit=line,fmt='(4I2)') (/ ( i, i = 1, ndfp ) /) + ndim
  if (line /= ' 3 4 5 6') call abort
end program t
! { dg-final { scan-tree-dump-times "__var" 3 "original" } }
