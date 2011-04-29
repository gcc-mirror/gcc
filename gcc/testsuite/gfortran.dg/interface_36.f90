! { dg-do compile }
!
! PR fortran/48800
!
! Contributed by Daniel Carrera
!
     pure function runge_kutta_step(t, r_, dr, h) result(res)
         real, intent(in) :: t, r_(:), h
         real, dimension(:), allocatable :: k1, k2, k3, k4, res
         integer :: N

         interface
             pure function dr(t, r_)  ! { dg-error "cannot have a deferred shape" }
                 real, intent(in) :: t, r_(:)
                 real :: dr(:)
             end function
         end interface

         N = size(r_)
         allocate(k1(N),k2(N),k3(N),k4(N),res(N))

         k1 = dr(t, r_)
         k2 = dr(t + h/2, r_ + k1*h/2)
         k3 = dr(t + h/2, r_ + k2*h/2)
         k4 = dr(t + h  , r_ + k3*h)

         res = r_ + (k1 + 2*k2 + 2*k3 + k4) * h/6
     end function
