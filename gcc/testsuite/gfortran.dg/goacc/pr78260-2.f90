! { dg-do compile }
! { dg-options "-fopenacc -fdump-tree-original" }
! { dg-require-effective-target fopenacc }

! PR fortran/78260

module m
  implicit none
  integer :: n = 0
contains
  integer function f1()
    !$acc declare present(f1)
    !$acc kernels copyin(f1)
    f1 = 5 
    !$acc end kernels
  end function f1
end module m
! { dg-final { scan-tree-dump-times "#pragma acc data map\\(force_present:__result_f1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma acc data map\\(force_present:__result_f1\\)" 1 "original" } }

