! The following directives are all 'pure' and should compile
! However, they are not yet implemented. Once done, move to pure-1.f90

!pure logical function func_declare_induction()
logical function func_declare_induction()
  implicit none
  ! Not quite right but should trigger an different error once implemented.
  !$omp declare induction(next : (integer, integer))   &  ! { dg-error "Unclassifiable OpenMP directive" }
  !$omp&        inductor (omp_var = omp_var(omp_step)) &
  !$omp&        collector(omp_step * omp_idx)

  func_declare_induction = .false.
end

!pure logical function func_interchange(n)
logical function func_interchange(n)
  implicit none
  integer, value :: n
  integer :: i, j
  func_interchange = .false.
  !$omp interchange permutation(2,1) ! { dg-error "Unclassifiable OpenMP directive" }
  do i = 1, n
    do j = 1, n
      func_interchange = .not. func_interchange
    end do
  end do
end

!pure logical function func_reverse(n)
logical function func_reverse(n)
  implicit none
  integer, value :: n
  integer :: j
  func_reverse = .false.
  !$omp reverse  ! { dg-error "Unclassifiable OpenMP directive" }
  do j = 1, n
    func_reverse = .not. func_reverse
  end do
end

