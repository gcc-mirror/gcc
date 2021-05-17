! PR fortran/100633

module defined_assign
  interface assignment(=)
    module procedure work_assign
  end interface

  contains
    subroutine work_assign(a,b)
      integer, intent(out) :: a
      logical, intent(in) :: b(:)
    end subroutine work_assign
end module defined_assign

program omp_workshare
  use defined_assign

  integer :: a
  logical :: l(10)
  l = .TRUE.

  !$omp workshare
  a = l   ! { dg-error "Expected intrinsic assignment in OMP WORKSHARE" }
  !$omp end workshare

end program omp_workshare
