! { dg-do compile }
! Check for overlapping case range diagnostics.
!
program select_5
  integer i
  select case(i)
  case (20:30)
  case (25:) ! { dg-error "overlaps with CASE" "" }
  end select
  select case(i)
  case (30)
  case (25:) ! { dg-error "overlaps with CASE" "" }
  end select
  select case(i)
  case (20:30)
  case (25) ! { dg-error "overlaps with CASE" "" }
  end select
end program select_5
