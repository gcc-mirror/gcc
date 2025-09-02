! { dg-do compile )
!
! Test the fix for pr84122
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module mod
type foo(idim)
  integer, len, PUBLIC :: idim ! { dg-error "is not allowed" }
  private
  integer :: array(idim)
end type
end module

module bar
type foo(idim)
  private
  integer,len :: idim         ! { dg-error "must come before a PRIVATE statement" }
  integer :: array(idim)
end type
end module
