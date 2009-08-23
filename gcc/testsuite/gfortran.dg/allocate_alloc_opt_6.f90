! { dg-do run }
program a

  implicit none

  type :: mytype
    real ::  r
    integer :: i
  end type mytype
  
  integer n
  integer, allocatable :: i(:)
  real z
  real, allocatable :: x(:)
  type(mytype), pointer :: t

  n = 42
  z = 99.

  allocate(i(4), source=n)
  if (any(i /= 42)) call abort

  allocate(x(4), source=z)
  if (any(x /= 99.)) call abort

  allocate(t, source=mytype(1.0,2))
  if (t%r /= 1. .or. t%i /= 2) call abort

  deallocate(i)
  allocate(i(3), source=(/1, 2, 3/))
  if (i(1) /= 1 .or. i(2) /= 2 .or. i(3) /= 3) call abort

  call sub1(i)

end program a

subroutine sub1(j)
   integer, intent(in) :: j(*)
   integer, allocatable :: k(:)
   allocate(k(2), source=j(1:2))
   if (k(1) /= 1 .or. k(2) /= 2) call abort
end subroutine sub1
