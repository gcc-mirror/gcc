! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-require-effective-target ilp32 }
! { dg-require-effective-target sse2 }
! { dg-options "-O2 -floop-strip-mine -fprefetch-loop-arrays -msse2 -fdump-tree-graphite-details --param graphite-allow-codegen-errors=1" }

subroutine blts ( ldmx, ldmy, v, tmp1, i, j, k)
  implicit none
  integer ldmx, ldmy, i, j, k, ip, m, l
  real*8 tmp, tmp1, v( 5, ldmx, ldmy, *), tmat(5,5)

  do ip = 1, 4
     do m = ip+1, 5
        tmp = tmp1 * tmat( m, ip )
        do l = ip+1, 5
           tmat( m, l ) =  tmat( m, l ) - tmat( ip, l )
        end do
        v( m, i, j, k ) = tmp
     end do
  end do
  return
end subroutine blts

subroutine phasad(t,i,ium)
  implicit none
  real t(5,4)
  integer i,l,ll,ium

  do l=1,2
     ll=2*l
     do i=1,ium
        t(i,ll-1)=t(i,ll-1)+t(i,ll)
     enddo
  enddo
  return
end subroutine phasad

! { dg-final { scan-tree-dump-times "code generation error" 1 " graphite" } }
