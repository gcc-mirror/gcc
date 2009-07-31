subroutine matrix_multiply(a,b,c,n)

real(8), dimension(n,n) :: a,b,c

do i = 1,n
  do j = 1,n
    do k = 1,n
      c(j,i) = c(j,i) + a(k,i) * b(j,k)
    enddo
 enddo
enddo

end subroutine matrix_multiply

! { dg-final { scan-tree-dump-times "Loop blocked" 2 "graphite" { xfail *-*-* } } } 
! { dg-final { cleanup-tree-dump "graphite" } } 

