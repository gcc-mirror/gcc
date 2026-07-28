! { dg-do compile }
!  ICE: a module procedure with an allocatable, intent(out)
!  derived-type array dummy. Reduced from the original in the PR

module m
  interface
    module subroutine s (a)
      integer, allocatable, intent(out) :: a
    end subroutine
  end interface
end module

submodule (m) sm1
contains
  module subroutine s (a)
    integer, allocatable, intent(out) :: a
    allocate (a, source = 7)
  end subroutine
end submodule

submodule (m:sm1) sm2
contains
  subroutine caller
    integer, allocatable :: b
    call s (b)
    if (b /= 7) stop 1
  end subroutine
end submodule
