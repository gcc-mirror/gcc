! { dg-do compile }

type t
end type t
type t2
  integer :: j = 7
end type t2
contains
  subroutine x(a, b, c)
    intent(out) :: a, b, c
    type(t) :: a = t()
    type(t2) :: b = t2()
    type(t2) :: c
  end subroutine x
end

! { dg-error "Dummy .a. at .1. cannot have an initializer" " " { target *-*-* } 9 }
! { dg-error "Dummy .b. at .1. cannot have an initializer" " " { target *-*-* } 9 }
