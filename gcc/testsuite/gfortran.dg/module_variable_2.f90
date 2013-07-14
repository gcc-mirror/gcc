! { dg-do compile }
! { dg-options "-Wall -fmodule-private" }

module bar
  integer :: i ! { dg-warning "Unused PRIVATE" }
end module bar
