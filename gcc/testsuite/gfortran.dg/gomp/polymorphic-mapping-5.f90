subroutine one
implicit none
type t
  class(*), allocatable :: ul
end type

class(*), allocatable :: ul_var
!$omp target enter data map(to: ul_var) ! { dg-error "Mapping of unlimited polymorphic list item 'ul_var' is unspecified behavior and unsupported" }
end
