! { dg-do  run }
! { dg-additional-options "-fdump-tree-original" }
! PR 45424 - compile-time simplification of is_contiguous
program main
  real, dimension(10,5) :: a
  character (len=1) :: line

  write (unit=line,fmt='(L1)') is_contiguous(a(4:2,:))
  if (line /= 'F') stop 1

  write (unit=line,fmt='(L1)') is_contiguous(a(:,2:4))
  if (line /= 'T') stop 1

  write (unit=line,fmt='(L1)') is_contiguous(a(2:4,3:4))
  if (line /= 'F') stop 3

  write (unit=line,fmt='(L1)') is_contiguous(a(::2,:))
  if (line /= 'F') stop 4

  write (unit=line,fmt='(L1)') is_contiguous(a(:,::2))
  if (line /= 'F') stop 5

end program main
! { dg-final { scan-tree-dump-not " _gfortran_is_contiguous" "original" } }
