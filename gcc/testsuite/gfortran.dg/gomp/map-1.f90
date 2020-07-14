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

  !$omp target map(j(1:9:2))
 ! { dg-error "Array is not contiguous" "" { target *-*-* } 60 }
 ! { dg-error "Stride should not be specified for array section in MAP clause" "" { target *-*-* } 60 }
  !$omp end target

  !$omp target map(aas(5:))
  !$omp end target
  ! { dg-error "Rightmost upper bound of assumed size array section not specified" "" { target *-*-* } 65 }
  ! { dg-error "'aas' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 65 }

  !$omp target map(aas(:))
  !$omp end target
  ! { dg-error "Rightmost upper bound of assumed size array section not specified" "" { target *-*-* } 70 }
  ! { dg-error "'aas' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 70 }

  !$omp target map(aas) ! { dg-error "Assumed size array" }
  !$omp end target

  !$omp target map(aas(5:7))
  !$omp end target

  !$omp target map(aas(:7))
  !$omp end target

  !$omp target map(k(5:))
  !$omp end target
  ! { dg-error "Rank mismatch in array reference" "" { target *-*-* } 84 }
  ! { dg-error "'k' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 84 }

  !$omp target map(k(5:,:,3))
  !$omp end target
  ! { dg-error "Rank mismatch in array reference" "" { target *-*-* } 89 }
  ! { dg-error "'k' in MAP clause at \\\(1\\\) is not a proper array section" "" { target *-*-* } 89 }

  !$omp target map(tt)
  !$omp end target

  !$omp target map(tt%k) ! { dg-error "not a member of" }
  !$omp end target ! { dg-error "Unexpected !\\\$OMP END TARGET statement" }

  !$omp target map(tt%j)
  !$omp end target

  !$omp target map(tt%j(1))
  !$omp end target

  !$omp target map(tt%j(1:))
  !$omp end target

  !$omp target map(tp) ! { dg-error "THREADPRIVATE object 'tp' in MAP clause" }
  !$omp end target
end subroutine test
