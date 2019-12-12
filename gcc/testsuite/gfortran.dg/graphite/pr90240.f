! { dg-do compile }
! { dg-options "-O1 -floop-nest-optimize" }

      PARAMETER (n=1335, N2=1335)
      COMMON  a(n,N2), b(n,N2), c(n,N2),
     *        d(n,N2),
     2        e(n,N2), f(n,N2),
     *        g(n,N2), h(n,N2)               
      DO 200 j=1,i
      DO 300 k=1,l
      a(k,j) = c(k,j)*g(k,j)*f(k+1,m)+f(k,m)+f(k,j)
     2       +f(k+1,j)*h(k+1,j)
      b(k,j+1) = d(k,j+1)*g(k,m)+g(k,j+1)
     1       *e(k,m)+e(k,j+1)+e(k,j)+e(k+1,j)
     2       *h(k,j+1)-h(k,j)
  300 ENDDO
  200 ENDDO
      END
