! { dg-do compile }
!
! Contributed by  G. Steinmetz
!
! PR fortran/93552
! only collapsed an not tile was checked:
program p
   integer :: i, j
   !$acc parallel loop tile(2,2)
   outer: do i = 1, 8
      do j = 1, 8
         exit  ! { dg-error "statement at .1. terminating ..ACC LOOP loop" }
         cycle outer ! { dg-error "to non-innermost tiled" }
      end do
   end do outer
end

! Kernels loop was missing the check:
subroutine test
  !$acc kernels loop collapse(2)
  outer: do i = 1, 4
    do j = 1, 4
      exit  ! { dg-error "statement at .1. terminating ..ACC LOOP loop" }
      cycle outer ! { dg-error "to non-innermost collapsed" }
    end do
  end do outer
end
