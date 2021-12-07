! { dg-do compile }
! PR fortran/103588 - ICE: Simplification error in gfc_ref_dimen_size
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(:) = [1,2] ! { dg-error "cannot be automatic or of deferred shape" }
  integer :: b(2) = a(::a(1))        ! { dg-error "Invalid" }
end
