! { dg-do compile }
! { dg-require-effective-target vect_float }

subroutine s243(ntimes,ld,n,ctime,dtime,a,b,c,d,e,aa,bb,cc)
      
integer ntimes,ld,n,i,nl
real a(n),b(n),c(n),d(n),e(n),aa(ld,n),bb(ld,n),cc(ld,n)
real t1,t2,chksum,ctime,dtime,cs1d
  b(:n-1)= b(:n-1)+(c(:n-1)+e(:n-1))*d(:n-1)
  a(:n-1)= b(:n-1)+a(2:n)*d(:n-1)
  return
end

! { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect"   } }
! { dg-final { cleanup-tree-dump "vect" } }

