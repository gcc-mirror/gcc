implicit none

integer :: a

!$omp target map(close, delete: a) ! { dg-error "TARGET with map-type other than TO, FROM, TOFROM, or ALLOC on MAP clause at \\(1\\)" }
!$omp end target

!$omp target map(close) ! { dg-error "Symbol 'close' at \\(1\\) has no IMPLICIT type" }
!$omp end target

!$omp target map(always) ! { dg-error "Symbol 'always' at \\(1\\) has no IMPLICIT type" }
!$omp end target

!$omp target map(always, always, to : a) ! { dg-error "too many 'always' modifiers" }
! !$omp end target
!$omp target map(always always, to : a) ! { dg-error "too many 'always' modifiers" }
! !$omp end target
!$omp target map(always, always to : a) ! { dg-error "too many 'always' modifiers" }
! !$omp end target
!$omp target map(always always to : a) ! { dg-error "too many 'always' modifiers" }
! !$omp end target
!$omp target map(close, close, to : a) ! { dg-error "too many 'close' modifiers" }
! !$omp end target
!$omp target map(close close, to : a) ! { dg-error "too many 'close' modifiers" }
! !$omp end target
!$omp target map(close, close to : a) ! { dg-error "too many 'close' modifiers" }
! !$omp end target
!$omp target map(close close to : a) ! { dg-error "too many 'close' modifiers" }
! !$omp end target

!$omp target map(present present, to : a) ! { dg-error "too many 'present' modifiers" }
! !$omp end target
!$omp target map(present, present to : a) ! { dg-error "too many 'present' modifiers" }
! !$omp end target
!$omp target map(present present to : a) ! { dg-error "too many 'present' modifiers" }
! !$omp end target


!$omp target map(close close always always to : a) ! { dg-error "too many 'always' modifiers" }
! !$omp end target

!$omp target map(present close always present to : a) ! { dg-error "too many 'present' modifiers" }
! !$omp end target

end
