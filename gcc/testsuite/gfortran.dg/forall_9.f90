! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
  integer a(100)
  forall (i=1:100,.false.)
      a(i) = 0
  end forall
  end
! { dg-final { scan-tree-dump-times "temp" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
