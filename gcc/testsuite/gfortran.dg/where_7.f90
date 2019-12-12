! { dg-do compile }
! { dg-options "-ffrontend-optimize" }
! PR fortran/88073 - this used to ICE with front-end optimization
! Original test case by 'mecej4'
Subroutine tfu (n, x, f)
   Implicit None
   Integer, Parameter :: double = Kind (0.d0)
   Integer, Intent (In) :: n
   Real (double), Intent (Out) :: f
   Real (double), Intent (In) :: x (n)
   Integer :: j
   Logical, Dimension(n) :: l1v, l2v, l3v
!
   l3v = .False.
   l2v = .False.
   l1v = (/ (j, j=1, n) /) == 1
   Where ( .Not. (l1v))
      l2v = (/ (j, j=1, n) /) == n
   End Where
   Where ( .Not. l1v)
      l3v = .Not. l2v
   End Where
   f = sum (x(1:n), mask=l3v)
   Return
end subroutine tfu
