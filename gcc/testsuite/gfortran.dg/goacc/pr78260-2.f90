! { dg-do compile }
! { dg-options "-fopenacc -fdump-tree-original" }
! { dg-require-effective-target fopenacc }

! PR fortran/78260

! Loosely related to PR fortran/94120

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
  integer function g1() result(g1res)
    !$acc declare present(g1res)
    !$acc kernels copyin(g1res)
    g1res = 5 
    !$acc end kernels
  end function g1
end module m
! { dg-final { scan-tree-dump-times "#pragma acc data map\\(force_present:__result_f1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma acc kernels map\\(to:__result_f1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma acc data map\\(force_present:g1res\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma acc kernels map\\(to:g1res\\)" 1 "original" } }
