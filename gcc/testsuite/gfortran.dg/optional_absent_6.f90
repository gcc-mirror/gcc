! { dg-do run }
! PR fortran/107441
!
! Test VALUE + OPTIONAL for integer/real/...
! in the presence of non-optional character dummies

program bugdemo
  implicit none
  character :: s = 'a'
  integer   :: t

  t = testoptional(s)
  call test2 (s)
  call test3 (s)
  call test4 (w='123',x=42)

contains

  function testoptional (w, x) result(t)
    character, intent(in)                  :: w
    integer,   intent(in), value, optional :: x
    integer :: t
    print *, 'present(x) is', present(x)
    t = 0
    if (present (x)) stop 1
  end function testoptional

  subroutine test2 (w, x)
    character, intent(in)                  :: w
    integer,   intent(in), value, optional :: x
    print*, 'present(x) is', present(x)
    if (present (x)) stop 2
  end subroutine test2

  subroutine test3 (w, x)
    character, intent(in),        optional :: w
    integer,   intent(in), value, optional :: x
    print *, 'present(w) is', present(w)
    print *, 'present(x) is', present(x)
    if (.not. present (w)) stop 3
    if (present (x)) stop 4
  end subroutine test3

  subroutine test4 (r, w, x)
    real,                     value, optional :: r
    character(*), intent(in),        optional :: w
    integer,                  value, optional :: x
    print *, 'present(r) is', present(r)
    print *, 'present(w) is', present(w)
    print *, 'present(x) is', present(x)
    if (present (r)) stop 5
    if (.not. present (w)) stop 6
    if (.not. present (x)) stop 7
    print *, 'x=', x
    print *, 'len(w)=', len(w)
    if (len(w) /= 3) stop 8
    if (x /= 42) stop 9
  end subroutine test4

end program bugdemo
