! { dg-do compile { target vect_simd_clones } }
! { dg-additional-options "-fdump-tree-gimple" }
! { dg-additional-options "-mno-sse3" { target { i?86-*-* x86_64-*-* } } }

program main
  implicit none
contains
  integer function f01 (x)
    integer, intent(in) :: x
    f01 = x
  end function

  integer function f02 (x)
    integer, intent(in) :: x
    f02 = x
  end function

  integer function f03 (x)
    integer, intent(in) :: x
    f03 = x
  end function

  integer function f04 (x)
    integer, intent(in) :: x
    f04 = x
  end function

  integer function f05 (x)
    integer, intent(in) :: x

    !$omp declare variant (f01) match (device={isa("avx512f")}) ! 4 or 8
    !$omp declare variant (f02) match (implementation={vendor(score(3):gnu)},device={kind(cpu)}) ! (1 or 2) + 3
    !$omp declare variant (f03) match (user={condition(score(9):.true.)})
    !$omp declare variant (f04) match (implementation={vendor(score(6):gnu)},device={kind(host)}) ! (1 or 2) + 6
    f05 = x
  end function

  integer function test1 (x)
    !$omp declare simd
    integer, intent(in) :: x

    ! 0 or 1 (the latter if in a declare simd clone) constructs in OpenMP context,
    ! isa has score 2^2 or 2^3.  We can't decide on whether avx512f will match or
    ! not, that also depends on whether it is a declare simd clone or not and which
    ! one, but the f03 variant has a higher score anyway.  */
    test1 = f05 (x)	! { dg-final { scan-tree-dump-times "f03 \\\(x" 1 "gimple" } }
  end function
end program
