! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/48858
! PR fortran/48820
!
! OPTIONAL + BIND(C) is allowed since TS 29113
!

! VALID
subroutine sub(z) bind(C)
  use iso_c_binding
  integer(c_int), value :: z
end subroutine sub

! VALID since TS29113
subroutine sub2(z) bind(C) ! { dg-error "with OPTIONAL attribute in procedure" }
  use iso_c_binding
  integer(c_int), optional :: z
end subroutine sub2

! VALID since TS29113
subroutine sub2a(z) bind(C) ! { dg-error "with OPTIONAL attribute in procedure" }
  use iso_c_binding
  integer(c_int) :: z
  optional :: z
end subroutine sub2a

! VALID since TS29113
subroutine sub2b(z) bind(C) ! { dg-error "with OPTIONAL attribute in procedure" }
  use iso_c_binding
  optional :: z
  integer(c_int) :: z
end subroutine sub2b

! Invalid
subroutine sub3(z) bind(C) ! { dg-error "cannot have both the OPTIONAL and the VALUE attribute" }
  use iso_c_binding
  integer(c_int), value, optional :: z
end subroutine sub3

! Invalid
subroutine sub3a(z) bind(C) ! { dg-error "cannot have both the OPTIONAL and the VALUE attribute" }
  use iso_c_binding
  integer(c_int) :: z
  optional :: z
  value :: z
end subroutine sub3a

! Invalid
subroutine sub3b(z) bind(C) ! { dg-error "cannot have both the OPTIONAL and the VALUE attribute" }
  use iso_c_binding
  optional :: z
  value :: z
  integer(c_int) :: z
end subroutine sub3b

! Invalid
subroutine sub3c(z) bind(C) ! { dg-error "cannot have both the OPTIONAL and the VALUE attribute" }
  use iso_c_binding
  value :: z
  integer(c_int) :: z
  optional :: z
end subroutine sub3c
