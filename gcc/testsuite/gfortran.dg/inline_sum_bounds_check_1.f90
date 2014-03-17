! { dg-do run }
! { dg-options "-fbounds-check -Wno-aggressive-loop-optimizations" }

      integer, parameter :: nx = 3, ny = 4

      integer :: i, too_big

      integer, parameter, dimension(nx,ny) :: p = &
        reshape((/ (i*i, i=1,size(p)) /), shape(p))

      integer, dimension(nx,ny) :: a

      integer, dimension(:), allocatable :: b

      allocate(b(nx))

      a = p
      too_big = ny + 1

      b = sum(a(:,1:too_big),2)
      end
! { dg-shouldfail "outside of expected range" }
