! PR middle-end/28046  for the original C tet.
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-ompexp" }
! { dg-require-effective-target cas_int }

module m
  implicit none
  integer a(3), b
  type t_C
     integer :: x, y
  end type
  type(t_C) :: c

  interface
    integer function bar(); end
    integer function baz(); end
  end interface
  pointer :: baz
contains
subroutine foo
!$omp atomic
  a(2) = a(2) + bar ()
!$omp atomic
  b = b + bar ()
!$omp atomic
  c%y = c%y + bar ()
!$omp atomic
  b = b + baz ()
end
end module

! { dg-final { scan-tree-dump-times "__atomic_fetch_add" 4 "ompexp" } }
