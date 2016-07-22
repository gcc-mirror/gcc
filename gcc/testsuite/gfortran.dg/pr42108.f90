! { dg-do compile }
! { dg-options "-O2 -fdump-tree-fre1 -fdump-tree-pre-details -fno-tree-loop-im" }

subroutine  eval(foo1,foo2,foo3,foo4,x,n,nnd)
  implicit real*8 (a-h,o-z)
  dimension foo3(n),foo4(n),x(nnd)
  nw=0
  foo3(1)=foo2*foo4(1)
  do i=2,n
    foo3(i)=foo2*foo4(i)
    do  j=1,i-1
      temp=0.0d0
      jmini=j-i
      do  k=i,nnd,n
        temp=temp+(x(k)-x(k+jmini))**2
      end do
      temp = sqrt(temp+foo1)
      foo3(i)=foo3(i)+temp*foo4(j)
      foo3(j)=foo3(j)+temp*foo4(i)
    end do
  end do
end subroutine eval

! There should be only one load from n left
! { dg-final { scan-tree-dump-times "\\*n_" 1 "fre1" } }
