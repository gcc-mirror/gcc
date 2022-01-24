! { dg-do compile }
! { dg-options "std=gnu" }
! PR fortran/103286 - ICE in resolve_select

program p
  select case (.true.) ! { dg-warning "Extension: Conversion" }
  case (1_8)
  case (:0)            ! { dg-error "Logical range in CASE statement" }
  case (2:)            ! { dg-error "Logical range in CASE statement" }
  end select
end
