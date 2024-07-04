! PR fortran/114825

subroutine pr114825(b)
  type t
    real, allocatable :: m(:)
  end type t
  type(t), allocatable, target :: b(:)
  type(t), pointer :: d
  !$omp parallel private(d)
  d => b(1)
  !$omp end parallel
contains
  subroutine sub
    d => b(1)
  end subroutine sub
end subroutine pr114825
