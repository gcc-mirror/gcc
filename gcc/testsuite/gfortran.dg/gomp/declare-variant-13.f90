! { dg-do compile { target vect_simd_clones } }
! { dg-additional-options "-O0 -fdump-tree-ompdevlow" }
! { dg-additional-options "-mno-sse3" { target { i?86-*-* x86_64-*-* } } }

module foo
contains
  integer function f01 (x)
    integer, intent(in) :: x
    f01 = x + 1
  end function

  integer function f02 (x)
    integer, intent(in) :: x
    f02 = x + 2
  end function

  integer function f03 (x)
    integer, intent(in) :: x
    f03 = x + 3
  end function

  integer function f04 (x)
    integer, intent(in) :: x
    f04 = x + 4
  end function

  integer function f05 (x)
    integer, intent(in) :: x

    !$omp declare variant (f01) match (device={isa("avx512f")}) ! 4 or 8
    !$omp declare variant (f02) match (implementation={vendor(score(3):gnu)},device={kind(cpu)}) ! (1 or 2) + 3
    !$omp declare variant (f03) match (user={condition(score(9):.true.)})
    !$omp declare variant (f04) match (implementation={vendor(score(6):gnu)},device={kind(host)}) ! (1 or 2) + 6
    f05 = x + 5
  end function
end module

program main
  use :: foo
  implicit none

  if (test1 (42) /= 42 + 3) stop 100
  
contains
  integer function test1 (x)
    !$omp declare simd
    integer, intent(in) :: x

    ! 0 or 1 (the latter if in a declare simd clone) constructs in OpenMP context,
    ! isa has score 2^2 or 2^3.  We can't decide on whether avx512f will match or
    ! not, that also depends on whether it is a declare simd clone or not and which
    ! one, but the f03 variant has a higher score anyway.  */
    test1 = f05 (x)
    ! { dg-final { scan-tree-dump "f03 \\\(x" "ompdevlow" } }
    ! { dg-final { scan-tree-dump-not "f05 \\\(x" "ompdevlow" } }
  end function

end program
