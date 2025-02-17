! { dg-additional-options "-fdump-tree-gimple" }
! { dg-final { scan-tree-dump-not "g \\(\\)" "gimple" } }
! { dg-final { scan-tree-dump "i = f \\(\\);" "gimple" } }

! PR fortran/115271

module m
interface
  integer function f ()
  end
  integer function g ()
    !$omp declare variant(f) match(construct={dispatch})
  end
end interface
end

use m
!$omp dispatch
  i = g()
end
