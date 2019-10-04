! { dg-do compile }

! PR fortran/78260

module m
  implicit none
  integer :: n = 0
contains
  subroutine s
    !$omp target data map(m)   ! { dg-error "Object .m. is not a variable" }
    !$omp target update to(m)  ! { dg-error "Object .m. is not a variable" }
    n = n + 1
    !$omp end target data
  end subroutine s
  subroutine s2
    !$omp target data map(s2)   ! { dg-error "Object .s2. is not a variable" }
    !$omp target update to(s2)  ! { dg-error "Object .s2. is not a variable" }
    n = n + 1
    !$omp end target data
  end subroutine s2
  integer function f1()
    !$omp target data map(f1)   ! OK, f1 is also the result variable
    !$omp target update to(f1)  ! OK, f1 is also the result variable
    f1 = 5 
    !$omp end target data
  end function f1
  integer function f2() result(res)
    !$omp target data map(f2)   ! { dg-error "Object .f2. is not a variable" }
    !$omp target update to(f2)  ! { dg-error "Object .f2. is not a variable" }
    res = 5 
    !$omp end target data
  end function f2
end module m
