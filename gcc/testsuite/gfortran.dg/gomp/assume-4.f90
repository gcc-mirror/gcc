! { dg-do compile }
! { dg-options "-fopenmp -O2 -fdump-tree-original -fdump-tree-optimized" }
! { dg-final { scan-tree-dump-times ".ASSUME \\(i_lower_bound \\(\\) < i\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times ".ASSUME \\(TARGET_EXPR <D.\[0-9\]+, D.\[0-9\]+ = j_upper_bound \\(\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 1 "original" } }

! { dg-final { scan-tree-dump-not "i_lower_bound" "optimized" } }
! { dg-final { scan-tree-dump-not "j_upper_bound" "optimized" } }
! { dg-final { scan-tree-dump-not "__builtin_free" "optimized" } }

! Note: Currently, the assumption does not help with optimization in either variant.


integer function f(i)
  implicit none
  integer, value :: i

  !$omp assume holds(i > i_lower_bound ())
  block
    if (i > 4) then
      f = 42
    else
      f = -1
    end if
  end block
contains
  function i_lower_bound ()
    integer :: i_lower_bound
    i_lower_bound = 5
  end function
end

integer function g(j)
  implicit none
  integer, value :: j

  !$omp assume holds(j < j_upper_bound ())
  block
    if (j < 10) then
      g = 42
    else
      g = -1
    end if
  end block
contains
  function j_upper_bound ()
    integer, allocatable :: j_upper_bound
    j_upper_bound = 10
  end function
end
