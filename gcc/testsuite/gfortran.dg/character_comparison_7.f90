! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Test that expressions in subroutine calls are also optimized
program main
  implicit none
  character(len=4) :: c
  c = 'abcd'
  call yes(c == c)
  call no(c /= c)
end program main

subroutine yes(a)
  implicit none
  logical, intent(in) :: a
  if (.not. a) call abort
end subroutine yes

subroutine no(a)
  implicit none
  logical, intent(in) :: a
  if (a) call abort
end subroutine no

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

