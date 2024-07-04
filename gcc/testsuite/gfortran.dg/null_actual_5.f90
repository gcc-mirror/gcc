! { dg-do compile }
! PR fortran/55978
!
! Passing of NULL() with and without MOLD as actual argument
!
! Testcase derived from pr55978 comment#16

program pr55978_c16
  implicit none

  integer, pointer       :: p(:)
  integer, allocatable   :: a(:)
  character(10), pointer :: c
  character(10), pointer :: cp(:)

  type t
    integer, pointer     :: p(:)
    integer, allocatable :: a(:)
  end type

  type(t) :: d

  ! (1) pointer
  p => null()
  call sub (p)

  ! (2) allocatable
  call sub (a)
  call sub (d%a)

  ! (3) pointer component
  d%p => null ()
  call sub (d%p)

  ! (4) NULL
  call sub (null (a))   ! OK
  call sub (null (p))   ! OK
  call sub (null (d%a)) ! OK
  call sub (null (d%p)) ! OK
  call sub (null ())    ! was erroneously rejected with:
  ! Actual argument contains too few elements for dummy argument 'x' (1/4)

  call bla (null(c))
  call bla (null())     ! was erroneously rejected with:
  ! Actual argument contains too few elements for dummy argument 'x' (1/10)

  call foo (null(cp))
  call foo (null())

  call bar (null(cp))
  call bar (null())     ! was erroneously rejected with:
  ! Actual argument contains too few elements for dummy argument 'x' (1/70)

contains

  subroutine sub(x)
    integer, intent(in), optional :: x(4)
    if (present (x)) stop 1
  end

  subroutine bla(x)
    character(len=10), intent(in), optional :: x
    if (present (x)) stop 2
  end

  subroutine foo(x)
    character(len=10), intent(in), optional :: x(:)
    if (present (x)) stop 3
  end

  subroutine bar(x)
    character(len=10), intent(in), optional :: x(7)
    if (present (x)) stop 4
  end

end
