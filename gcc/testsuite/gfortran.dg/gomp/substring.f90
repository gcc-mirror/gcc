implicit none
character(len=10) :: str1, str2(5,5)

type t
  character(len=10) :: str1, str2(5,5)
end type t
type(t) :: v

!$omp target enter data map(to: str1)      ! OK
!$omp target enter data map(to: str2)      ! OK
!$omp target enter data map(to: str2(2,5)) ! OK

!$omp target enter data map(to: str1(2,5))         ! { dg-error "Syntax error in OpenMP variable list" }
!$omp target enter data map(to: str2(1,2)(2:4))    ! { dg-error "Unexpected substring reference in MAP clause" }

!$omp target enter data map(to: v%str1)       ! OK
!$omp target enter data map(to: v%str2)       ! OK
!$omp target enter data map(to: v%str2(1,2))  ! OK

!$omp target enter data map(to: v%str1(2:5))       ! { dg-error "Unexpected substring reference in MAP clause" }
!$omp target enter data map(to: v%str2(1,2)(2:4))  ! { dg-error "Unexpected substring reference in MAP clause" }
end
