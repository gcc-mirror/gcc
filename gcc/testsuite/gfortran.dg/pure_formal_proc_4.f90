! { dg-do compile }
! PR fortran/106948 - check that passing of PURE procedures works
!
! Contributed by Jim Feng

module a
  implicit none

  interface new
    pure module subroutine b(x, f)
      integer, intent(inout) :: x
      interface
        pure function f(x) result(r)
          real, intent(in) :: x
          real :: r
        end function f
      end interface
    end subroutine b
  end interface new
end module a

submodule(a) a_b
  implicit none

contains
  module procedure b
    x = int(f(real(x)) * 0.15)
  end procedure b
end submodule a_b

program test
  use a
  implicit none

  integer :: x

  x = 100
  call new(x, g)
  print *, x

contains

  pure function g(y) result(r)
    real, intent(in) :: y
    real :: r

    r = sqrt(y)
  end function g
end program test
