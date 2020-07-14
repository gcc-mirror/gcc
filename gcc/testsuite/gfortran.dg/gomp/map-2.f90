type t
  integer :: i
end type t
type(t) v
!$omp target enter data map(to:v%i, v%i)  ! { dg-error "appears more than once in map clauses" }
end
