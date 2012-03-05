! { dg-do compile }
!
! PR fortran/52059
!
!

subroutine baz
  real(kind=8) :: a(99), b
  interface bar
    function bar (x, y)
      integer, intent(in) :: x, y
      real(kind=8), dimension((y-x)) :: bar
    end function bar
  end interface
  b = 1.0_8
  a = foo (bar(0,35) / dble(34), b)
contains
  elemental real(kind=8) function foo(x, y)
    real(kind=8), intent(in) :: x, y
    foo = 1
  end function foo
end subroutine baz
