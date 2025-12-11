! { dg-do compile }
subroutine test(b,c,d)
  implicit none
  integer, value, target :: b
  integer, pointer :: c
  integer, allocatable, target :: d

  integer, target :: a(5)

  !$omp target is_device_ptr(a) ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use HAS_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }

  !$omp end target

  !$omp target is_device_ptr(b) ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use HAS_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }
  !$omp end target

  !$omp target is_device_ptr(c) ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use HAS_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }
  !$omp end target

  !$omp target is_device_ptr(d) ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use HAS_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }
  !$omp end target

  !$omp target data map(a) use_device_addr(a)  ! Should be okay
  !$omp end target data

  !$omp target data map(c) use_device_ptr(c)  ! { dg-warning "Non-C_PTR type argument at \\(1\\) is deprecated, use USE_DEVICE_ADDR \\\[-Wdeprecated-openmp\\\]" }
  !$omp end target data
end subroutine test
