! { dg-do compile }
! PR fortran/99036 - ICE in gfc_current_interface_head
! Contributed by G. Steinmetz

module m
contains
  module procedure s ! { dg-error "must be in a generic module interface" }
  end
end
