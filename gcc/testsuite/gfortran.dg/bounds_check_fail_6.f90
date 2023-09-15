! { dg-do run }
! { dg-additional-options "-fcheck=bounds -g -fdump-tree-original" }
! { dg-output "At line 18 .*" }
! { dg-shouldfail "dimension 3 of array 'u%z' outside of expected range" }
!
! PR fortran/30802 - improve bounds-checking for array sections

program test
  implicit none
  integer :: k = 0
  integer, dimension(10,20,30) :: x = 42
  type t
     real, dimension(10,20,30) :: z = 23
  end type t
  type(t) :: u

  ! pr30802
  print *, u% z(1,:,k)  ! runtime check only for dimension 3

  ! pr97039
  call foo (x(k,:,k+1)) ! runtime checks for dimensions 1,3
contains
  subroutine foo (a)
    integer, intent(in) :: a(:)
  end subroutine foo
end program test

! { dg-final { scan-tree-dump-times "'u%%z.' outside of expected range" 2 "original" } }
! { dg-final { scan-tree-dump-times "'x.' outside of expected range" 4 "original" } }
