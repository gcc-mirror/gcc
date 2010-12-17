! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR fortran/20865
  subroutine tt(j)
   integer :: j
  end subroutine

  integer :: i, st
  st(i) = (i*i+2)
  call tt(st) ! { dg-error "Statement function .* is not allowed as an actual argument|Invalid procedure argument" }
  end
