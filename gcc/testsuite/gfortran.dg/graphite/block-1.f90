subroutine matrix_multiply(a,b,c,n)

real(8), dimension(n,n) :: a,b,c

! The following code is disabled for the moment.
c=0.d0

end subroutine matrix_multiply

! Disabled for now as it requires delinearization.
! { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite" { xfail *-*-* } } }
