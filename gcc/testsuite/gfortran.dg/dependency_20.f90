! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
  integer :: a(4)

  where (a(:) .ne. 0)
    a(:) = (/ 1, 2, 3, 4 /)
  endwhere
end
! { dg-final { scan-tree-dump-times "temp" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
