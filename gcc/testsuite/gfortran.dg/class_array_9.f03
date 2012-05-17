! { dg-do run }
! Test typebound elemental functions on class arrays
!
module m
  type :: t1
    integer :: i
  contains
    procedure, pass :: disp => disp_t1
  end type t1

  type, extends(t1) :: t2
    real :: r
  contains
    procedure, pass :: disp => disp_t2
  end type t2

contains
  integer elemental function disp_t1 (q)
    class(t1), intent(in) :: q
    disp_t1 = q%i
  end function

  integer elemental function disp_t2 (q)
    class(t2), intent(in) :: q
    disp_t2 = int (q%r)
  end function
end module

  use m
  class(t1), allocatable :: x(:)
  allocate (x(4), source = [(t1 (i), i=1,4)])
  if (any (x%disp () .ne. [1,2,3,4])) call abort
  if (any (x(2:3)%disp () .ne. [2,3])) call abort
  if (any (x(4:3:-1)%disp () .ne. [4,3])) call abort
  if (x(4)%disp () .ne. 4) call abort

  deallocate (x)
  allocate (x(4), source = [(t2 (2 * i, real (i) + 0.333), i=1,4)])
  if (any (x%disp () .ne. [1,2,3,4])) call abort
  if (any (x(2:3)%disp () .ne. [2,3])) call abort
  if (any (x(4:3:-1)%disp () .ne. [4,3])) call abort
  if (x(4)%disp () .ne. 4) call abort

end
