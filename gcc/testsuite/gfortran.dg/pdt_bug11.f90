! { dg-do run }
! See attachment to PR82173 comment 10 - reproducers/bug11-class-dummy-extension-len.f90
module m4
  type :: base(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    real(k) :: v(n)
  end type
  type, extends(base) :: child
    integer :: extra = 5
  end type
contains
  subroutine show (c)
    class(base(4, *)), intent(in) :: c
    select type (c)
    type is (base(4, *))
      if (c%n /= 3) stop 1
      if (size (c%v) /= 3) stop 2
    type is (child(4, *))
      if (c%n /= 5) stop 3
      if (size (c%v) /= 5) stop 4
      if (c%extra /= 42) stop 5
    class default
      stop 6
    end select
  end subroutine
end module
program i4
  use m4
  type(base(4, 3)) :: b
  type(child(4, 5)) :: c
  b%v = 0.0
  c%v = 0.0
  c%extra = 42
  call show (b)
  call show (c)
end program
