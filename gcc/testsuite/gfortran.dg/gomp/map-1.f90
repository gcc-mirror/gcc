subroutine test(aas)
  implicit none

  integer :: i, j(10), k(10, 10), aas(*)
  integer, save :: tp
  !$omp threadprivate(tp)
  integer, parameter :: p = 1

  type t
    integer :: i, j(10)
  end type t

  type(t) :: tt

  !$omp target map(i)
  !$omp end target

  !$omp target map(j)
  !$omp end target

  !$omp target map(p) ! { dg-error "Object 'p' is not a variable" }
  !$omp end target

  !$omp target map(j(1))
  !$omp end target

  !$omp target map(j(i))
  !$omp end target

  !$omp target map(j(i:))
  !$omp end target

  !$omp target map(j(:i))
  !$omp end target

  !$omp target map(j(i:i+1))
  !$omp end target

  !$omp target map(j(11)) ! { dg-warning "out of bounds" }
  !$omp end target

  !$omp target map(j(:11)) ! { dg-warning "out of bounds" }
  !$omp end target

  !$omp target map(j(0:)) ! { dg-warning "out of bounds" }
  !$omp end target

  !$omp target map(j(5:4))
  !$omp end target

  !$omp target map(j(5:))
  !$omp end target

  !$omp target map(j(:5))
  !$omp end target

  !$omp target map(j(:))
  !$omp end target

  !$omp target map(j(1:9:2)) ! { dg-error "Stride should not be specified for array section in MAP clause" }
  !$omp end target

  !$omp target map(aas(5:))
  !$omp end target
  ! { dg-error "Rightmost upper bound of assumed size array section not specified" "" { target *-*-* } 63 }
  ! { dg-error "'aas' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 63 }

  !$omp target map(aas(:))
  !$omp end target
  ! { dg-error "Rightmost upper bound of assumed size array section not specified" "" { target *-*-* } 68 }
  ! { dg-error "'aas' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 68 }

  !$omp target map(aas) ! { dg-error "Assumed size array" }
  !$omp end target

  !$omp target map(aas(5:7))
  !$omp end target

  !$omp target map(aas(:7))
  !$omp end target

  !$omp target map(k(5:))
  !$omp end target
  ! { dg-error "Rank mismatch in array reference" "" { target *-*-* } 82 }
  ! { dg-error "'k' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 82 }

  !$omp target map(k(5:,:,3))
  !$omp end target
  ! { dg-error "Rank mismatch in array reference" "" { target *-*-* } 87 }
  ! { dg-error "'k' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 87 }

  !$omp target map(tt)
  !$omp end target

  !$omp target map(tt%i) ! { dg-error "Syntax error in OpenMP variable list" }
  !$omp end target ! { dg-error "Unexpected !\\\$OMP END TARGET statement" }

  !$omp target map(tt%j) ! { dg-error "Syntax error in OpenMP variable list" }
  !$omp end target ! { dg-error "Unexpected !\\\$OMP END TARGET statement" }

  ! broken test
  !$omp target map(tt%j(1)) ! { dg-error "Syntax error in OpenMP variable list" }
  !$omp end target ! { dg-error "Unexpected !\\\$OMP END TARGET statement" }

  !$omp target map(tt%j(1:)) ! { dg-error "Syntax error in OpenMP variable list" }
  !$omp end target ! { dg-error "Unexpected !\\\$OMP END TARGET statement" }

  !$omp target map(tp) ! { dg-error "THREADPRIVATE object 'tp' in MAP clause" }
  !$omp end target
end subroutine test
