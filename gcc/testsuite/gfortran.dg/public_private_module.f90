! { dg-do compile }
! See PR fortran/36251.
module a
  implicit none
  integer :: i = 42
end module a

module b
  use a
  implicit none
  public a  ! { dg-error "attribute applied to" }
end module b

module d
  use a
  implicit none
  private a  ! { dg-error "attribute applied to" }
end module d
