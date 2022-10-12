! { dg-do compile }
! { dg-options "-fopenmp -O2 -fdump-tree-optimized -fdump-tree-original" }

! { dg-final { scan-tree-dump-times ".ASSUME \\(x == 42\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times ".ASSUME \\(x <= 41\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times ".ASSUME \\(y <= 6\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times ".ASSUME \\(y > 5\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "return 42;" 3 "optimized" } }
! { dg-final { scan-tree-dump-not "return -1;" "optimized" } }

integer function foo (x)
  implicit none
  integer, value :: x
  integer :: y
  !$omp assume holds (x == 42)
    y = x;
  !$omp end assume
  foo = y
end

integer function bar (x)
  implicit none
  integer, value :: x
  !$omp assume holds (x < 42)
  block
  end block
  if (x == 42) then
    bar = -1
    return
  end if
  bar = 42
end

integer function foobar (y)
  implicit none
  integer, value :: y
  !$omp assume holds(y > 5) holds (y < 7)
  block
    if (y == 6) then
      foobar = 42
      return
    end if
  end block
  foobar = -1
end
