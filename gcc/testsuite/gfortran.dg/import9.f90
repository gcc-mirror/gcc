! { dg-do compile }
!
! PR fortran/48821
!
! Contributed by Daniel Carrera
!

contains
     pure subroutine rk4_vec(t, Y, dY, h)
         real, intent(inout) :: t, Y(:)
         real, intent(in) :: h
         real, dimension(size(Y)) :: k1, k2, k3, k4

         interface
             pure function dY(t0, y0)
                 import :: Y
                 real, intent(in) :: t0, y0(size(Y))
                 real :: dY(size(y0))
             end function
         end interface

         k1 = dY(t, Y)
         k2 = dY(t + h/2, Y + k1*h/2)
         k3 = dY(t + h/2, Y + k2*h/2)
         k4 = dY(t + h  , Y + k3*h)

         Y = Y + (k1 + 2*k2 + 2*k3 + k4) * h/6
         t = t + h
     end subroutine
end
