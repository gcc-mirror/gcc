! { dg-do run }
! { dg-options "-fbounds-check" }

      integer, parameter :: nx = 3, ny = 4

      integer :: i, j, too_big

      integer, parameter, dimension(nx,ny) :: p = &
        reshape((/ (i*i, i=1,size(p)) /), shape(p))

      integer, dimension(nx,ny) :: a

      integer, dimension(:), allocatable :: c


      allocate(c(ny))

      a = p
      too_big = nx + 1

      c = sum(a(1:too_big,:),2)
      end
! { dg-shouldfail "outside of expected range" }
