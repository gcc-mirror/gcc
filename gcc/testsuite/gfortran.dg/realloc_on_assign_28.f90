! { dg-do run }
!
! PR fortran/66102
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
  type t
    integer,allocatable :: i
  end type

  type(t) :: e
  type(t), allocatable, dimension(:) :: a, b
  integer :: chksum = 0

  do i=1,3   ! Was 100 in original
    e%i = i
    chksum = chksum + i
    if (.not.allocated(a)) then
      a = [e]
      b = first_arg([e], [e])
    else
      call foo
    end if
  end do

  if (sum ([(a(i)%i, i=1,size(a))]) .ne. chksum) STOP 1
  if (any([(a(i)%i, i=1,size(a))] /= [(i, i=1,size(a))])) STOP 2
  if (size(a) /= size(b)) STOP 3
  if (any([(b(i)%i, i=1,size(b))] /= [(i, i=1,size(b))])) STOP 4
contains
  subroutine foo
    b = first_arg([b, e], [a, e])
    a = [a, e]
  end subroutine
  elemental function first_arg(arg1, arg2)
    type(t), intent(in) :: arg1, arg2
    type(t)             :: first_arg
    first_arg = arg1
  end function first_arg
end
