! { dg-do compile }
! { dg-additional-options "-Wunused-label" }

! Check that a format label defined in the first statement after a nested
! metadirective body can be referenced correctly.

integer :: cnt, x
cnt = 0
!$omp begin metadirective when(user={condition(cnt > 0)} : parallel)
  !$omp begin metadirective when(user={condition(cnt > 0)} : parallel)
    x = 5
  !$omp end metadirective
  1234 format("Hello")
  write(*,1234)
!$omp end metadirective
end
