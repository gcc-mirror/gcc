! { dg-do compile }

! Check that redefining labels across metadirective regions triggers a
! diagnostic.

implicit none
integer :: cnt
1345 format("The count is ", g0)

cnt = 0
write(*,1345) cnt

!$omp begin metadirective when(user={condition(cnt > 0)} : parallel)
  6789 format("The count is ", g0)
  !$omp begin metadirective when(user={condition(cnt > 0)} : parallel)
    1345 print *, 'nested'  ! { dg-error "Label 1345 at .1. already referenced as a format label" }
    6789 print *, 'world'
  !$omp end metadirective
  write(*,1345) cnt    ! { dg-error "Label 1345 at .1. previously used as branch target" }
  write(*,6789) cnt    ! { dg-error "Label 6789 at .1. previously used as branch target" }
!$omp end metadirective
end
