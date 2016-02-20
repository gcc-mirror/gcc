! { dg-do compile }
! PR fortran/52531
module test_mod
  type, public :: test_type
  end type
contains
  subroutine foo(bar)
    type(test_type) :: bar
!$omp parallel default(none) shared(bar) ! Compiles if one removes default(none)
    call question(bar)
!$omp end parallel
  end subroutine
  subroutine question(var)
    class(test_type), intent(in) :: var ! Compiles if one replaces class by type
  end subroutine
end module
