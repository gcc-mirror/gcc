! { dg-do compile }
! { dg-options "-O2 -fno-tree-vectorize -ffast-math -fdump-tree-pre" }

      subroutine foo(U,V,R,N,A)
      integer N
      real*8 U(N,N,N),V(N,N,N),R(N,N,N),A(0:3)
      integer I3, I2, I1
C
      do I3=2,N-1
       do I2=2,N-1
        do I1=2,N-1
         R(I1,I2,I3)=V(I1,I2,I3)
     *      -A(0)*( U(I1,  I2,  I3  ) )
     *      -A(1)*( U(I1-1,I2,  I3  ) + U(I1+1,I2,  I3  )
     *                 +  U(I1,  I2-1,I3  ) + U(I1,  I2+1,I3  )
     *                 +  U(I1,  I2,  I3-1) + U(I1,  I2,  I3+1) )
     *      -A(2)*( U(I1-1,I2-1,I3  ) + U(I1+1,I2-1,I3  )
     *                 +  U(I1-1,I2+1,I3  ) + U(I1+1,I2+1,I3  )
     *                 +  U(I1,  I2-1,I3-1) + U(I1,  I2+1,I3-1)
     *                 +  U(I1,  I2-1,I3+1) + U(I1,  I2+1,I3+1)
     *                 +  U(I1-1,I2,  I3-1) + U(I1-1,I2,  I3+1)
     *                 +  U(I1+1,I2,  I3-1) + U(I1+1,I2,  I3+1) )
     *      -A(3)*( U(I1-1,I2-1,I3-1) + U(I1+1,I2-1,I3-1)
     *                 +  U(I1-1,I2+1,I3-1) + U(I1+1,I2+1,I3-1)
     *                 +  U(I1-1,I2-1,I3+1) + U(I1+1,I2-1,I3+1)
     *                 +  U(I1-1,I2+1,I3+1) + U(I1+1,I2+1,I3+1) )
        enddo
       enddo
      enddo
      return
      end

! PRE shouldn't do predictive commonings job here (and in a bad way)
! ???  It still does but not as bad as it could.  Less prephitmps
! would be better, pcom does it with 6.
! { dg-final { scan-tree-dump-times "# prephitmp" 9 "pre" } }
