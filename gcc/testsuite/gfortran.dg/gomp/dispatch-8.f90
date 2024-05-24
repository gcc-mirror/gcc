! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! Check that, when the novariants or nocontext clauses cannot be evaluated at
! compile time, both variants are emitted.

module main
  use iso_c_binding, only: c_ptr
  implicit none
  interface
  integer function f0 ()
  end function
  integer function f1 ()
  end function
  integer function f2 ()
    !$omp declare variant (f0) match (construct={dispatch})
    !$omp declare variant (f1) match (implementation={vendor(gnu)})
  end function
  end interface
  contains
  
  subroutine test ()
    integer :: a, n

  !$omp dispatch novariants(n < 1024) nocontext(n > 1024)
    a = f2 ()
  end subroutine
end module

! { dg-final { scan-tree-dump-times "D\.\[0-9]+ = n <= 1023;" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9]+ = n > 1024;" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch novariants\\(0\\) nocontext\\(0\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "a = f2 \\\(\\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "a = f1 \\\(\\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "a = f0 \\\(\\\);" 1 "gimple" } }

