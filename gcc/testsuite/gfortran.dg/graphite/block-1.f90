subroutine matrix_multiply(a,b,c,n)

real(8), dimension(n,n) :: a,b,c

! The following code is disabled for the moment.
! c=0.d0

end subroutine matrix_multiply

! { dg-final { scan-tree-dump-times "Loop blocked" 2 "graphite" { xfail *-*-* } } } 
! { dg-final { cleanup-tree-dump "graphite" } } 

