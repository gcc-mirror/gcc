! { dg-do compile }
!
! PR fortran/46413
!
type t
  integer :: ii =5
end type t
class(t), allocatable :: x
allocate (t :: x)

print *,x  ! { dg-error "Data transfer element at .1. cannot be polymorphic" }
end
