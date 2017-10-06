! { dg-do compile }
! { dg-require-effective-target vect_double }
! { dg-options "-O3 --param vect-max-peeling-for-alignment=0 -fpredictive-commoning -fdump-tree-pcom-details" }
! { dg-additional-options "-mprefer-avx128" { target { i?86-*-* x86_64-*-* } } }
! { dg-additional-options "-mzarch" { target { s390*-*-* } } }

******* RESID COMPUTES THE RESIDUAL:  R = V - AU
*
*      THIS SIMPLE IMPLEMENTATION COSTS  27A + 4M PER RESULT, WHERE
*      A AND M DENOTE THE COSTS OF ADDITION (OR SUBTRACTION) AND 
*      MULTIPLICATION, RESPECTIVELY.  BY USING SEVERAL TWO-DIMENSIONAL 
*      BUFFERS ONE CAN REDUCE THIS COST TO  13A + 4M IN THE GENERAL 
*      CASE, OR  10A + 3M WHEN THE COEFFICIENT A(1) IS ZERO.
*
      SUBROUTINE RESID(U,V,R,N,A)
      INTEGER N
      REAL*8 U(N,N,N),V(N,N,N),R(N,N,N),A(0:3)
      INTEGER I3, I2, I1
C
      DO 600 I3=2,N-1
      DO 600 I2=2,N-1
      DO 600 I1=2,N-1
 600  R(I1,I2,I3)=V(I1,I2,I3)
     >      -A(0)*( U(I1,  I2,  I3  ) )
     >      -A(1)*( U(I1-1,I2,  I3  ) + U(I1+1,I2,  I3  )
     >                 +  U(I1,  I2-1,I3  ) + U(I1,  I2+1,I3  )
     >                 +  U(I1,  I2,  I3-1) + U(I1,  I2,  I3+1) )
     >      -A(2)*( U(I1-1,I2-1,I3  ) + U(I1+1,I2-1,I3  )
     >                 +  U(I1-1,I2+1,I3  ) + U(I1+1,I2+1,I3  )
     >                 +  U(I1,  I2-1,I3-1) + U(I1,  I2+1,I3-1)
     >                 +  U(I1,  I2-1,I3+1) + U(I1,  I2+1,I3+1)
     >                 +  U(I1-1,I2,  I3-1) + U(I1-1,I2,  I3+1)
     >                 +  U(I1+1,I2,  I3-1) + U(I1+1,I2,  I3+1) )
     >      -A(3)*( U(I1-1,I2-1,I3-1) + U(I1+1,I2-1,I3-1)
     >                 +  U(I1-1,I2+1,I3-1) + U(I1+1,I2+1,I3-1)
     >                 +  U(I1-1,I2-1,I3+1) + U(I1+1,I2-1,I3+1)
     >                 +  U(I1-1,I2+1,I3+1) + U(I1+1,I2+1,I3+1) )
C
      RETURN
      END
! we want to check that predictive commoning did something on the
! vectorized loop.  If vector factor is 2, the vectorized loop can
! be predictive commoned, we check if predictive commoning PHI node
! is created with vector(2) type.
! { dg-final { scan-tree-dump "Executing predictive commoning without unrolling" "pcom" } }
! { dg-final { scan-tree-dump "vectp_u.*__lsm.* = PHI <.*vectp_u.*__lsm" "pcom" } }
