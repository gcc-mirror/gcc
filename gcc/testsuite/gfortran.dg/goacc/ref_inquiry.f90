! Check for <var>%re, ...%im, ...%kind, ...%len
! Cf. also OpenMP's ../gomp/ref_inquiry.f90
! Cf. OpenACC spec issue 346
! 
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
!$acc enter data copyin(i%kind, c%len)     ! { dg-error "not a proper array section" }
!$acc enter data copyin(x%i%kind)          ! { dg-error "not a proper array section" }
!$acc enter data copyin(x%c%len)           ! { dg-error "not a proper array section" }
!$acc update self(i%kind, c%len)           ! { dg-error "not a proper array section" }
!$acc update self(x%i%kind)                ! { dg-error "not a proper array section" }
!$acc update self(x%c%len)                 ! { dg-error "not a proper array section" }

! EXPR_VARIABLE
!$acc enter data copyin(z%re)    ! { dg-error "Unexpected complex-parts designator" }
!$acc enter data copyin(z%im)    ! { dg-error "Unexpected complex-parts designator" }
!$acc enter data copyin(zz%re)   ! { dg-error "not a proper array section" }
!$acc enter data copyin(zz%im)   ! { dg-error "not a proper array section" }

!$acc enter data copyin(x%z%re)  ! { dg-error "Unexpected complex-parts designator" }
!$acc enter data copyin(x%z%im)  ! { dg-error "Unexpected complex-parts designator" }
!$acc enter data copyin(x%zz%re) ! { dg-error "not a proper array section" }
!$acc enter data copyin(x%zz%im) ! { dg-error "not a proper array section" }

!$acc update self(z%re)         ! { dg-error "Unexpected complex-parts designator" }
!$acc update self(z%im)         ! { dg-error "Unexpected complex-parts designator" }
!$acc update self(zz%re)        ! { dg-error "not a proper array section" }
!$acc update self(zz%im)        ! { dg-error "not a proper array section" }

!$acc update self(x%z%re)       ! { dg-error "Unexpected complex-parts designator" }
!$acc update self(x%z%im)       ! { dg-error "Unexpected complex-parts designator" }
!$acc update self(x%zz%re)      ! { dg-error "is not a proper array section" }
!$acc update self(x%zz%im)      ! { dg-error "is not a proper array section" }
end
