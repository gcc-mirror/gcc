! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original -fdump-tree-optimized -fsanitize=undefined" }
!
! PR fortran/122080 - UBSAN: uninitialized stride for missing actual argument
!
! Contributed by Henri Menke

subroutine outer (optarr)
  real, optional, intent(in) :: optarr(:,:)
  interface
     subroutine inner (optarr)
       real, optional, intent(in) :: optarr(:,:)
     end subroutine inner
  end interface
  call inner (optarr)
end subroutine outer

! There will be 2 remaining UBSAN checks of stride wrapped by a check
! for argument presence:
!
! { dg-final { scan-tree-dump-times "if \\(optarr.0 != 0B\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "UBSAN_CHECK_SUB (.)* stride" 2 "optimized" } }
