! { dg-do run }
!
! Check whether registering allocatable coarrays works
!
type position
  real :: x, y, z
end type position

integer, allocatable :: a[:]
type(position), allocatable :: p[:]

allocate(a[*])
a = 7

allocate(p[*])
p%x = 11
p%y = 13
p%z = 15

if (a /= 7) STOP 1
a = 88
if (a /= 88) STOP 2

if (p%x /= 11) STOP 3
p%x = 17
if (p%x /= 17) STOP 4

 block
   integer, allocatable :: b[:]

   allocate(b[*])
   b = 8494
   
   if (b /= 8494) STOP 5
 end block

if (a /= 88) STOP 6
call test ()
end

subroutine test()
  type velocity
    real :: x, y, z
  end type velocity

  real, allocatable :: z[:]
  type(velocity), allocatable :: v[:]

  allocate(z[*])
  z = sqrt(2.0)

  allocate(v[*])
  v%x = 21
  v%y = 23
  v%z = 25

  if (z /= sqrt(2.0)) STOP 7
  if (v%x /= 21) STOP 8

end subroutine test
