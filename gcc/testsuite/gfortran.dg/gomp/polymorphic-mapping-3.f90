subroutine sub(var, var2)
type t
  integer :: x
end type t

type t2
  integer :: x
  integer, allocatable :: y
end type

class(t) var, var2
type(t2) :: var3, var4
!$omp target firstprivate(var) &  ! { dg-error "Polymorphic list item 'var' at .1. in FIRSTPRIVATE clause has unspecified behavior and unsupported" }
!$omp&       private(var2)        ! { dg-error "Polymorphic list item 'var2' at .1. in PRIVATE clause has unspecified behavior and unsupported" }
   var%x = 5
   var2%x = 5
!$omp end target
!$omp target firstprivate(var3) &  ! { dg-error "Sorry, list item 'var3' at .1. with allocatable components is not yet supported in FIRSTPRIVATE clause" }
!$omp&       private(var4)         ! { dg-error "Sorry, list item 'var4' at .1. with allocatable components is not yet supported in PRIVATE clause" }
   var3%x = 5
   var4%x = 5
!$omp end target
end
