! { dg-do compile }
! { dg-options "-fopenmp -fmax-errors=100" }
! { dg-require-effective-target tls }

subroutine foo (ia1)
integer :: i1, i2, i3
integer, dimension (*) :: ia1
integer, dimension (10) :: ia2
real :: r1
real, dimension (5) :: ra1
double precision :: d1
double precision, dimension (4) :: da1
complex :: c1
complex, dimension (7) :: ca1
logical :: l1
logical, dimension (3) :: la1
character (5) :: a1
type t
  integer :: i
end type
type(t) :: t1
type(t), dimension (2) :: ta1
real, pointer :: p1 => NULL()
integer, allocatable :: aa1 (:,:)
save i2
!$omp threadprivate (i2)
common /blk/ i1

!$omp parallel reduction (+:i3, ia2, r1, ra1, d1, da1, c1, ca1)
!$omp end parallel
!$omp parallel reduction (*:i3, ia2, r1, ra1, d1, da1, c1, ca1)
!$omp end parallel
!$omp parallel reduction (-:i3, ia2, r1, ra1, d1, da1, c1, ca1)
!$omp end parallel
!$omp parallel reduction (.and.:l1, la1)
!$omp end parallel
!$omp parallel reduction (.or.:l1, la1)
!$omp end parallel
!$omp parallel reduction (.eqv.:l1, la1)
!$omp end parallel
!$omp parallel reduction (.neqv.:l1, la1)
!$omp end parallel
!$omp parallel reduction (min:i3, ia2, r1, ra1, d1, da1)
!$omp end parallel
!$omp parallel reduction (max:i3, ia2, r1, ra1, d1, da1)
!$omp end parallel
!$omp parallel reduction (iand:i3, ia2)
!$omp end parallel
!$omp parallel reduction (ior:i3, ia2)
!$omp end parallel
!$omp parallel reduction (ieor:i3, ia2)
!$omp end parallel
!$omp parallel reduction (+:/blk/)	! { dg-error "Syntax error" }
!$omp end parallel			! { dg-error "Unexpected" }
!$omp parallel reduction (+:i2)		! { dg-error "THREADPRIVATE object" }
!$omp end parallel
!$omp parallel reduction (*:p1)		! { dg-error "POINTER object" }
!$omp end parallel
!$omp parallel reduction (-:aa1)	! { dg-error "is ALLOCATABLE" }
!$omp end parallel
!$omp parallel reduction (*:ia1)	! { dg-error "Assumed size" }
!$omp end parallel
!$omp parallel reduction (+:l1)		! { dg-error "is LOGICAL" }
!$omp end parallel
!$omp parallel reduction (*:la1)	! { dg-error "is LOGICAL" }
!$omp end parallel
!$omp parallel reduction (-:a1)		! { dg-error "is CHARACTER" }
!$omp end parallel
!$omp parallel reduction (+:t1)		! { dg-error "is TYPE" }
!$omp end parallel
!$omp parallel reduction (*:ta1)	! { dg-error "is TYPE" }
!$omp end parallel
!$omp parallel reduction (.and.:i3)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.or.:ia2)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.eqv.:r1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.neqv.:ra1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.and.:d1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.or.:da1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.eqv.:c1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.neqv.:ca1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.and.:a1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.or.:t1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (.eqv.:ta1)	! { dg-error "must be LOGICAL" }
!$omp end parallel
!$omp parallel reduction (min:c1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (max:ca1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (max:l1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (min:la1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (max:a1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (min:t1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (max:ta1)	! { dg-error "must be INTEGER or REAL" }
!$omp end parallel
!$omp parallel reduction (iand:r1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ior:ra1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ieor:d1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ior:da1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (iand:c1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ior:ca1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ieor:l1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (iand:la1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ior:a1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (ieor:t1)	! { dg-error "must be INTEGER" }
!$omp end parallel
!$omp parallel reduction (iand:ta1)	! { dg-error "must be INTEGER" }
!$omp end parallel

end subroutine
