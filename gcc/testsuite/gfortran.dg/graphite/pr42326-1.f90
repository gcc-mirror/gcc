! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-require-effective-target ilp32 }
! { dg-require-effective-target sse2 }
! { dg-options "-O2 -floop-parallelize-all -fprefetch-loop-arrays -msse2" }

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

