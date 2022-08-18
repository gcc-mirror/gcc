! { dg-do compile }
!
! PR fortran/106566
!

subroutine add_one2(p)
  implicit none
  procedure(add_one2) :: ext1
  !$omp declare simd(ext1) linear(p: ref) simdlen(8)  ! { dg-error "OMP DECLARE SIMD should refer to containing procedure 'add_one2'" }
  integer :: p

  p = p + 1
end subroutine

subroutine linear_add_one2(p)
  implicit none
  procedure(linear_add_one2) :: ext2
  !$omp declare simd(ext2) linear(p: ref, step(2)) simdlen(8)  ! { dg-error "OMP DECLARE SIMD should refer to containing procedure 'linear_add_one2'" }
  integer :: p

  p = p + 1
end subroutine

module m
   integer, parameter :: NN = 1023
   integer :: a(NN)
contains
  subroutine some_proc(r)
    integer :: r
  end subroutine
  subroutine module_add_one2(q)
    implicit none
    !$omp declare simd(some_proc) linear(q: ref) simdlen(8)  ! { dg-error "OMP DECLARE SIMD should refer to containing procedure 'module_add_one2'" }
    integer :: q
    q = q + 1
  end subroutine

  subroutine module_linear_add_one2(q)
    implicit none
    interface
      subroutine other_proc(r)
        integer :: r
      end subroutine
    end interface
    !$omp declare simd(other_proc) linear(q: ref, step(2)) simdlen(8)  ! { dg-error "OMP DECLARE SIMD should refer to containing procedure 'module_linear_add_one2'" }
    integer :: q
    q = q + 1
  end subroutine
end module
