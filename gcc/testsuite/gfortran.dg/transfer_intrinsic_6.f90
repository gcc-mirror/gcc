! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 58058: [4.7/4.8/4.9 Regression] Memory leak with transfer function
!
! Contributed by Thomas Jourdan <thomas.jourdan@orange.fr>

  implicit none

  integer, dimension(3) :: t1
  character(len=64) :: str
  
  t1 = (/1,2,3/)

  str = transfer(t1,str)

end

! { dg-final { scan-tree-dump-times "__builtin_free" 1 "original" } }
