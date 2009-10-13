! { dg-do compile }
!
! PR fortran/41582
!
subroutine test()
type :: t
end type t
class(t), allocatable :: c,d
allocate(t :: d)
allocate(c,source=d)
end

type, abstract :: t
end type t
type t2
  class(t), pointer :: t
end type t2

class(t), allocatable :: a,c,d
type(t2) :: b
allocate(a) ! { dg-error "requires a type-spec or SOURCE" }
allocate(b%t) ! { dg-error "requires a type-spec or SOURCE" }
end
