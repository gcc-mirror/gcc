! { dg-do compile }
! { dg-options "-fopenmp" }

module m2
  implicit none (type, external)

  integer :: val

contains
integer function step (x)
  integer, value :: x
end
subroutine foo(x)
  integer, value :: x
  !$omp declare simd linear (val (x) : step (1))	! { dg-error "requires a constant integer linear-step expression or dummy argument" }
end
end module m2


module m
  implicit none (type, external)

  integer :: val

contains
integer function step (x)
  integer, value :: x
  !$omp declare simd linear (val (x) : step (1))	! { dg-error "Failed to match clause" }
end

integer function bar (x, y, z)
  integer, value :: x, y, z
  !$omp declare simd linear (val (x) : val)		! { dg-error "requires a constant integer linear-step expression or dummy argument" }
end

integer function baz (x, y, z)
  integer, value :: x, y, z
end
end module m
