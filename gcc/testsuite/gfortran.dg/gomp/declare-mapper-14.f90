program myprog
type T
integer :: arr1(10)
integer :: arr2(10)
end type T

type U
integer :: arr1(10)
end type U

type V
integer :: arr1(10)
end type V

!$omp declare mapper (default: T :: x) map(to:x%arr1) map(from:x%arr2)  ! { dg-error "Previous \\\!\\\$OMP DECLARE MAPPER" }
!$omp declare mapper (T :: x) map(to:x%arr1)  ! { dg-error "Redefinition of \\\!\\\$OMP DECLARE MAPPER" }

! Check what happens if we're SHOUTING too.
!$omp declare mapper (default: U :: x) map(to:x%arr1)  ! { dg-error "Previous \\\!\\\$OMP DECLARE MAPPER" }
!$omp declare mapper (DEFAULT: U :: x) map(to:x%arr1)  ! { dg-error "Redefinition of \\\!\\\$OMP DECLARE MAPPER" }

! Or if we're using a keyword (which should be fine).
!$omp declare mapper (V :: x) map(alloc:x%arr1)
!$omp declare mapper (integer : V :: x) map(tofrom:x%arr1(:))

end program myprog
