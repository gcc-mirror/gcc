! { dg-do compile }
! { dg-options "-fopenmp" }

module m
implicit none
integer, parameter :: val = 1
integer, parameter :: ref = 2
integer, parameter :: uval = 3

interface
  integer function foo (x, y, z)
    import
    implicit none
    integer, value :: x
    integer :: y, z
    !$omp declare simd linear (val (x) : step (1)) linear (ref (y) : step (2)) linear (uval (z) : step (3))

! STEP is a function - thus:
! { dg-error "'x' in LINEAR clause at .1. requires a constant integer linear-step expression or dummy argument specified in UNIFORM clause" "" { target *-*-* } .-3 }
! { dg-error "'y' in LINEAR clause at .1. requires a constant integer linear-step expression or dummy argument specified in UNIFORM clause" "" { target *-*-* } .-4 }
! { dg-error "'z' in LINEAR clause at .1. requires a constant integer linear-step expression or dummy argument specified in UNIFORM clause" "" { target *-*-* } .-5 }

  end

  integer function bar (x, y, z)
    import
    implicit none
    integer, value :: x
    integer :: y, z
    !$omp declare simd linear (val (x) : val) linear (ref (y) : ref) linear (uval (z) : uval)
  end

  integer function baz (x, y, z)
    import
    implicit none
    integer, value :: x
    integer :: y, z
    !$omp declare simd linear (val (x) : ref) linear (ref (y) : uval) linear (uval (z) : val)
  end

  integer function qux (x, y, z)
    import
    implicit none
    integer, value :: x
    integer :: y, z
    !$omp declare simd linear (val (x) : uval) linear (ref (y) : val) linear (uval (z) : ref)
  end
end interface
contains
  integer function step (x)
    integer, value :: x
     step = x
  end
end module
