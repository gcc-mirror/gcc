! PR fortran/45636
! { dg-do compile }
! { dg-options "-O2 -fdump-tree-forwprop2" }
! PR 45636 - make sure no memset is needed for a short right-hand side.
program main
  character(len=2), parameter :: x='a '
  character(len=1), parameter :: y='b'
  character(len=4) :: a, b
  a = x
  b = y
  call sub(a, b)
end program main
! { dg-final { scan-tree-dump-times "memset" 0 "forwprop2" { xfail { mips*-*-* && { ! nomips16 } } } } }
! { dg-final { cleanup-tree-dump "forwprop2" } }
