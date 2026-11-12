! { dg-do compile }
! PR 128678
!
! Addressable decls local to a SIMD body get an implicit private clause;
! the body initializes and finalizes them, so the OpenMP constructor hook
! must not.  This used to ICE in gfc_omp_clause_default_ctor.

module m
  implicit none
  type t
    real, allocatable :: v(:)
  end type
  type pair
    type(t) :: i, o
  end type
  type batch
    type(pair), allocatable :: p(:)
  end type
contains
  elemental function mkpair (i, o) result (r)
    type(t), intent(in) :: i, o
    type(pair) :: r
    r%i = i
    r%o = o
  end function
  pure function mkbatch (p) result (r)
    type(pair), intent(in) :: p(:)
    type(batch) :: r
    r%p = p
  end function
end module

subroutine temps (a, b)
  use m
  implicit none
  type(t) :: a(4, 3)
  type(batch) :: b(3)
  integer :: j
  !$omp parallel do simd
  do j = 1, 3
    b(j) = mkbatch (mkpair (a(:,j), a(:,j)))
  end do
end subroutine

subroutine block_locals (r)
  use m
  implicit none
  real :: r(100)
  integer :: j
  !$omp simd
  do j = 1, 100
    block
      real, allocatable :: w(:)
      type(t) :: x
      allocate (w(j))
      w = 1.0
      x%v = w
      call use_it (x%v, w)
      r(j) = sum (x%v)
    end block
  end do
contains
  subroutine use_it (a, b)
    real :: a(:), b(:)
  end subroutine
end subroutine
