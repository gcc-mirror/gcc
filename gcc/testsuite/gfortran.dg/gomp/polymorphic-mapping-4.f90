subroutine one
implicit none
type t
  class(*), allocatable :: ul
end type

type(t) :: var
!$omp target enter data map(to:var)  ! { dg-error "Mapping of unlimited polymorphic list item 'var.ul' is unspecified behavior and unsupported" }
end
