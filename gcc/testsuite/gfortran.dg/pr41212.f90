! { dg-do run }
! { dg-options "-O2" }
program m
   double precision :: y,z
   call b(1.0d0,y,z)
   if (ABS (z - 1.213) > 0.1) STOP 1
contains
     subroutine b( x, y, z)
       implicit none
       double precision :: x,y,z
       integer :: i, k
       double precision :: h, r

       y = 1.0d0
       z = 0.0d0

       h = 0
       DO k = 1,10
          h = h + 1.0d0/k

          r = 1
          DO i = 1,k
             r = (x/(2*i) ) * r
          END DO

          y = y + (-1)**k * r
          z = z + (-1)**(k+1) * h * r

          IF ( ABS(2*k/x*r) < 1d-6 ) EXIT
       END DO

       z = 2*y
     end subroutine b
end program m
