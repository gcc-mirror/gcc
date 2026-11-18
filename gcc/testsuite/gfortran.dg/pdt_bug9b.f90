! { dg-do run }
!
! PR 82173
!
module m
  type t(n)
    integer, len :: n
    integer :: a(n)
    character(n) :: s
  contains
    final :: fin
  end type
  type, extends(t) :: e
    integer :: b(n)
  end type
  integer :: nfin = 0
contains
  subroutine fin (x)
    type(t(*)), intent(inout) :: x
    nfin = nfin + 1
  end subroutine
  subroutine s_out (x)
    type(t(3)), intent(out) :: x
    x%a = 7
    x%s = 'abc'
  end subroutine
  subroutine s_out_ext (x)
    type(e(3)), intent(out) :: x
    x%a = 5
    x%b = 6
  end subroutine
end module
program p
  use m
  type(t(3)) :: y
  type(e(3)) :: z
  y%a = 1
  call s_out (y)
  if (any (y%a /= 7) .or. y%s /= 'abc' .or. nfin /= 1) stop 1
  y = t(3)([4,5,6], 'def')
  if (any (y%a /= [4,5,6]) .or. y%s /= 'def') stop 2
  call s_out_ext (z)
  if (any (z%a /= 5) .or. any (z%b /= 6)) stop 3
end program
