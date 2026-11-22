! { dg-do run }
!
! PR 128856

module m
  implicit none
  integer :: nfin = 0
  type :: c
    integer :: j = 0
  contains
    procedure :: asg
    generic :: assignment(=) => asg
  end type
  type :: t
    integer :: i = 0
  contains
    final :: fin_t
  end type
  type :: u
    type(c) :: comp
    integer :: i = 0
  contains
    final :: fin_u
  end type
contains
  subroutine asg (a, b)
    class(c), intent(out) :: a
    class(c), intent(in) :: b
    a%j = b%j
  end subroutine
  subroutine fin_t (x)
    type(t), intent(inout) :: x
    nfin = nfin + 1
  end subroutine
  subroutine fin_u (x)
    type(u), intent(inout) :: x
    nfin = nfin + 1
  end subroutine
  function make_t (i) result (r)
    integer, intent(in) :: i
    type(t) :: r
    r%i = i
  end function
  function make_u (i) result (r)
    integer, intent(in) :: i
    type(u) :: r
    r%i = i
  end function
  integer function get_t (x)
    type(t), intent(in) :: x
    get_t = x%i
  end function
  integer function get_u (x)
    type(u), intent(in) :: x
    get_u = x%i
  end function
  subroutine sub_t (x)
    type(t), intent(in) :: x
  end subroutine
  subroutine sub_u (x)
    type(u), intent(in) :: x
  end subroutine
end module

program p
  use m
  implicit none
  integer :: s, k
  type(u) :: a
  character(8) :: buf

  nfin = 0
  s = get_t (make_t (1))
  if (nfin /= 1) stop 1
  nfin = 0
  s = 1 + get_t (make_t (1))
  if (nfin /= 1) stop 2
  nfin = 0
  call sub_t (make_t (1))
  if (nfin /= 1) stop 3
  nfin = 0
  if (get_t (make_t (1)) /= 1) stop 4
  if (nfin /= 1) stop 5
  nfin = 0
  if (get_t (make_t (1)) == 1) then
    s = 0
  end if
  if (nfin /= 1) stop 7
  nfin = 0
  k = 0
  do while (get_t (make_t (k)) < 2)
    k = k + 1
  end do
  if (nfin /= 3) stop 8
  nfin = 0
  select case (get_t (make_t (2)))
  case (2)
    s = 0
  case default
    stop 10
  end select
  if (nfin /= 1) stop 11
  nfin = 0
  write (buf, '(i0)') get_t (make_t (1))
  if (nfin /= 1) stop 12

  nfin = 0
  s = get_u (make_u (1))
  if (nfin /= 1) stop 13
  nfin = 0
  s = 1 + get_u (make_u (1))
  if (nfin /= 1) stop 14
  nfin = 0
  call sub_u (make_u (1))
  if (nfin /= 1) stop 15
  nfin = 0
  if (get_u (make_u (1)) /= 1) stop 16
  if (nfin /= 1) stop 17
  nfin = 0
  a = make_u (1)
  if (nfin /= 2) stop 18
end program
