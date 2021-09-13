implicit none
type t
  integer :: b(5)
end type t
integer :: a(5), x, y(5)
type(t) :: b
!$omp target enter data map( to: a (:) )
!$omp target enter data map( to: b % b )
!$omp target enter data map( to: a(:) )
!$omp target depend(out: y (2)) nowait
!$omp end target

end
