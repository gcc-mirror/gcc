! { dg-do compile }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 52861 - optimize this to c = '' so that there is
! no memcpy in the generated code.
program main
  character (len=20) :: c
  c = '     '
  print *,c
end program main
! { dg-final { scan-tree-dump-times "memcpy" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
