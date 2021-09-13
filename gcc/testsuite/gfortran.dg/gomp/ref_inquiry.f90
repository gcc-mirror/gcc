! Check for <var>%re, ...%im, ...%kind, ...%len
! Cf. also OpenACC's ../goacc/ref_inquiry.f90
! Cf. also OpenMP spec issue 2661
implicit none
type t
  integer :: i
  character :: c
  complex :: z
  complex :: zz(5)
end type t

integer :: i
character(kind=4, len=5) :: c
complex :: z, zz(5)
type(t) :: x

print *, is_contiguous(zz(:)%re)

! inquiry function; expr_type != EXPR_VARIABLE:
!$omp target enter data map(to: i%kind, c%len)     ! { dg-error "not a proper array section" }
!$omp target enter data map(to: x%i%kind)          ! { dg-error "not a proper array section" }
!$omp target enter data map(to: x%c%len)           ! { dg-error "not a proper array section" }

! EXPR_VARIABLE
!$omp target enter data map(to: z%re)    ! { dg-error "Unexpected complex-parts designator" }
!$omp target enter data map(to: z%im)    ! { dg-error "Unexpected complex-parts designator" }
!$omp target enter data map(to: zz%re)   ! { dg-error "not a proper array section" }
!$omp target enter data map(to: zz%im)   ! { dg-error "not a proper array section" }

!$omp target enter data map(to: x%z%re)  ! { dg-error "Unexpected complex-parts designator" }
!$omp target enter data map(to: x%z%im)  ! { dg-error "Unexpected complex-parts designator" }
!$omp target enter data map(to: x%zz%re) ! { dg-error "not a proper array section" }
!$omp target enter data map(to: x%zz%im) ! { dg-error "not a proper array section" }

end
