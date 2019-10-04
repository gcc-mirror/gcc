! { dg-do compile }
! { dg-options "-fopenacc" }
! { dg-require-effective-target fopenacc }

! PR fortran/78260
! Contributed by Gerhard Steinmetz

module m
  implicit none
  integer :: n = 0
contains
  subroutine s
    !$acc declare present(m)  ! { dg-error "Object .m. is not a variable" }
    !$acc kernels copyin(m)   ! { dg-error "Object .m. is not a variable" }
    n = n + 1
    !$acc end kernels
  end subroutine s
  subroutine s2
    !$acc declare present(s2)  ! { dg-error "Object .s2. is not a variable" }
    !$acc kernels copyin(s2)   ! { dg-error "Object .s2. is not a variable" }
    n = n + 1
    !$acc end kernels
  end subroutine s2
  integer function f1()
    !$acc declare present(f1)  ! OK, f1 is also the result variable
    !$acc kernels copyin(f1)   ! OK, f1 is also the result variable
    f1 = 5 
    !$acc end kernels
  end function f1
  integer function f2() result(res)
    !$acc declare present(f2)  ! { dg-error "Object .f2. is not a variable" }
    !$acc kernels copyin(f2)   ! { dg-error "Object .f2. is not a variable" }
    res = 5 
    !$acc end kernels
  end function f2
end module m
