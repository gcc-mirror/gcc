! { dg-do compile }
! { dg-additional-options "-Wunused-label" }

! Check that a format label defined outside a metadirective body can be
! referenced correctly inside the metadirective body.

implicit none
integer :: cnt
1345 format("The count is ", g0)

cnt = 0
write(*,1345) cnt

!$omp begin metadirective when(user={condition(cnt > 0)} : parallel)
  write(*,1345) cnt
!$omp end metadirective
end
