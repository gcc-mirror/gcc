! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-require-effective-target vect_intfloat_cvt }
! { dg-additional-options "-ffast-math" }

program calcpi

    implicit none
    real(kind=4):: h,x,sum,pi
    integer:: n,i
    real(kind=4):: f

   f(x) = 4.0/(1.0+x**2)

   n = 2100000000

   h= 1.0 / n
   sum = 0.0
  DO i=1, n
     x = h * (i-0.5)
     sum = sum + f(x)
  END DO
  pi = h * sum
  write(*,*) 'Pi=',pi

end program calcpi

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } }
