! { dg-do run }
!
! PR fortran/127448
! Check the span set in array pointer assignments.
! For class pointers, the size from the class container was always
! used as span, which caused problems when the pointer was pointing
! to a parent subobject of an extended derived type entity.

program prog
  implicit none
  integer, parameter :: k = 2
  integer, parameter :: n = 5
  type :: t1
    integer(kind=k) :: c1, c2
  end type
  type, extends(t1) :: t2
    integer(kind=k) :: c3
  end type
  type(t2), target :: x(n)
  class(t1), pointer :: y1(:), z1(:)
  class(t2), pointer :: y2(:)
  type(t1), pointer :: p1(:), q1(:)
  integer :: i
  x = [ (t2(i*i, 2*i*i+1, i), i=1,n) ]
  y2 => x
  y1 => x
  call check_t1(y1, 12)
  z1 => y2
  call check_t1(z1, 13)
  z1 => y1
  call check_t1(z1, 14)
  call check_t1(x%t1, 21)
  call check_t1(y2%t1, 22)
  y1 => x%t1
  call check_t1(y1, 23)
  z1 => y2%t1
  call check_t1(z1, 24)
  z1 => y1
  call check_t1(z1, 25)
  p1 => x%t1
  call check_t1(p1, 41)
  p1 => y2%t1
  call check_t1(p1, 42)
  p1 => y1
  call check_t1(p1, 43)
  q1 => p1
  call check_t1(q1, 44)
contains
  subroutine check_t1(arg, f)
    type(t1), target, intent(in) :: arg(:)
    integer, intent(in) :: f
    call check_int(arg%c1, [1, 4,  9, 16, 25], f*10+1)
    call check_int(arg%c2, [3, 9, 19, 33, 51], f*10+2)
  end subroutine
  subroutine check_int(arg, e, f)
    integer(kind=k), intent(in) :: arg(:)
    integer, intent(in) :: e(:), f
    integer :: i
    if (size(arg, 1) /= size(e, 1)) error stop f*10+1
    !do i=1,size(arg)
    !  print *, f*10+i, (arg(i) == e(i) ? "PASS" : "FAIL"), arg(i), e(i)
    !end do
    if (any(arg /= e)) error stop f*10+2
  end subroutine
end program
