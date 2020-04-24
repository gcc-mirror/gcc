! { dg-do run }
! PR 93956 - span was set incorrectly, leading to wrong code.
! Original test case by "martin".
program array_temps
  implicit none
  
  type :: tt
     integer :: u = 1
     integer :: v = 2
  end type tt

  type(tt), dimension(:), pointer :: r
  integer :: n
  integer, dimension(:), pointer :: p, q, u

  n = 10
  allocate(r(1:n))
  call foo(r%v,n)
  p => get(r(:))
  call foo(p, n)
  call get2(r,u)
  call foo(u,n)
  q => r%v
  call foo(q, n)

deallocate(r)

contains

   subroutine foo(a, n)
      integer, dimension(:), intent(in) :: a
      integer, intent(in) :: n
      if (sum(a(1:n)) /= 2*n) stop 1
   end subroutine foo

   function get(x) result(q)
      type(tt), dimension(:), target, intent(in) :: x
      integer, dimension(:), pointer :: q
      q => x(:)%v
   end function get

   subroutine get2(x,q)
      type(tt), dimension(:), target, intent(in) :: x
      integer, dimension(:), pointer, intent(out) :: q
      q => x(:)%v
    end subroutine get2
end program array_temps
