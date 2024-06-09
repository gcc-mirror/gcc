! { dg-do compile }
! { dg-additional-options "-O3 -ffast-math" }
! { dg-additional-options "-mavx -mveclibabi=svml" { target i?86-*-* x86_64-*-* } }
subroutine foo (a, b, c, d, e, f, g, h, k, l)
  implicit none
  integer :: d, e, f, g, i, j
  real :: a, b(5,6), c(6), h(6,10,5), k(5,10,2), l(10,5), m, n, o
  do i=1,5
    do j=1,6
      m=l(f,g)*log(c(j))
      if (m<2) then
        if (m<-2) then
          h(j,f,g)=n
        else
          h(j,f,g)=o
        endif
      endif
      b(i,j)=a+k(i,d,e)+k(i,1,e)**h(j,f,g)
    enddo
  enddo
  write(*,'()') 
end
