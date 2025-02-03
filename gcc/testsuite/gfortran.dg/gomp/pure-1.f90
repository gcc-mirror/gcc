! The following directives are all 'pure' and should compile

pure logical function func_assume(i)
  implicit none
  integer, value :: i
  !$omp assume holds(i > 5)
    func_assume = i < 3
  !$omp end assume
end

pure logical function func_assumes()
  implicit none
  !$omp assumes absent(parallel)
  func_assumes = .false.
end

pure logical function func_reduction()
  implicit none
  !$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
  func_reduction = .false.
end

pure logical function func_declare_simd()
  implicit none
  !$omp declare simd
  func_declare_simd = .false.
end

pure logical function func_declare_target()
  implicit none
  !$omp declare target
  func_declare_target = .false.
end

pure logical function func_error_1()
  implicit none
  !$omp error severity(warning)  ! { dg-warning "OMP ERROR encountered" }
  func_error_1 = .false.
end

pure logical function func_error_2()
  implicit none
  !$omp error severity(warning) at(compilation)  ! { dg-warning "OMP ERROR encountered" }
  func_error_2 = .false.
end

pure logical function func_error_3()
  implicit none
  !$omp error severity(warning) at(execution)  ! { dg-error "OpenMP ERROR directive at .1. with 'at\\(execution\\)' clause in a PURE procedure" }
  func_error_3 = .false.
end

pure logical function func_nothing()
  implicit none
  !$omp nothing
  func_nothing = .false.
end

pure logical function func_scan(n)
  implicit none
  integer, value :: n
  integer :: i, r
  integer :: A(n)
  integer :: B(n)
  A = 0
  B = 0
  r = 0
  !$omp simd reduction (inscan, +:r)
  do i = 1, 1024
    r = r + a(i)
    !$omp scan inclusive(r)
    b(i) = i
  end do

  func_scan = b(1) == 3
end

pure integer function func_simd(n)
  implicit none
  integer, value :: n
  integer :: j, r
  r = 0
  !$omp simd reduction(+:r)
  do j = 1, n
    r = r + j
  end do
  func_simd = r
end

pure integer function func_unroll(n)
  implicit none
  integer, value :: n
  integer :: j, r
  r = 0
  !$omp unroll partial(2)
  do j = 1, n
    r = r + j
  end do
  func_unroll = r
end

pure integer function func_tile(n)
  implicit none
  integer, value :: n
  integer :: j, r
  r = 0
  !$omp tile sizes(2)
  do j = 1, n
    r = r + j
  end do
  func_tile = r
end

pure logical function func_metadirective()
  implicit none
  !$omp metadirective
  func_metadirective = .false.
end

! not 'parallel' not pure -> invalid in 5.2; + in general invalid in 5.1
pure logical function func_metadirective_2 ()
  implicit none
  integer :: i, n
  n = 0
  !$omp metadirective when (device={arch("nvptx")} : parallel do)     ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
  do i = 1, 5
    n = n + i
  end do
end

! unroll is supposed to be pure, so this case is OK
pure logical function func_metadirective_3()
  implicit none
  integer :: i, n

  n = 0
  !$omp metadirective when(device={arch("nvptx")} : unroll full)
  do i = 1, 5
    n = n + i
  end do
end
