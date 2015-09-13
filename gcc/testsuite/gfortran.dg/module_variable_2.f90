! { dg-do compile }
! { dg-options "-Wall -fmodule-private" }
! { dg-require-visibility "" }

module bar
  integer :: i ! { dg-warning "Unused PRIVATE" }
end module bar
