! { dg-do compile }
! { dg-options "-std=f2018" }

program selected
  implicit none

  logical(selected_logical_kind(1)) :: l ! { dg-error "has no IMPLICIT type" }
  print *, selected_logical_kind(1) ! { dg-error "has no IMPLICIT type" }
end program
