! { dg-do run }
! Test cshift where the values are eight bytes,
! but are aligned on a four-byte boundary.  The
! integers correspond to NaN values.
program main
  implicit none
  integer :: i
  type t
     sequence
     integer :: a,b
  end type t
  type(t), dimension(4) :: u,v
  common /foo/ u, i, v

  u(1)%a = 2142240768
  u(2)%a = 2144337920
  u(3)%a = -5242880
  u(4)%a = -3145728
  u%b = (/(i,i=-1,-4,-1)/)
  v(1:3:2) = cshift(u(1:3:2),1)
  v(2:4:2) = cshift(u(2:4:2),-1)
  if (any(v%a /= (/-5242880, -3145728, 2142240768, 2144337920 /))) call abort
  if (any(v%b /= (/-3, -4, -1, -2/))) call abort
end program main
