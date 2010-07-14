! { dg-do compile }
!
! PR 44925: [OOP] C_LOC with CLASS pointer
!
! Contributed by Barron Bichon <barron.bichon@swri.org>

  use iso_c_binding

  type :: t
  end type t

  type(c_ptr) :: tt_cptr
  class(t), pointer :: tt_fptr
  if (associated(tt_fptr)) tt_cptr = c_loc(tt_fptr)  ! { dg-error "must not be polymorphic" }

end
