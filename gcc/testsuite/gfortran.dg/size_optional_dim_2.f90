! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/113245 - SIZE, optional DIM argument, w/ OPTIONAL+VALUE attributes

program p
  implicit none
  real    :: a(2,3)
  integer :: expect
  expect = size (a,2)
  call ref (a,2)
  call val (a,2)
  expect = size (a)
  call ref (a)
  call val (a)
contains
  subroutine ref (x, dim)
    real,              intent(in) :: x(:,:)
    integer, optional, intent(in) :: dim
    print *, "present(dim), size(a,dim) =", present (dim), size (x,dim=dim)
    if (size (x,dim=dim) /= expect) stop 1
  end
  subroutine val (x, dim)
    real,              intent(in) :: x(:,:)
    integer, optional, value      :: dim
    print *, "present(dim), size(a,dim) =", present (dim), size (x,dim=dim)
    if (size (x,dim=dim) /= expect) stop 2
  end
end

! Ensure inline code is generated:
! { dg-final { scan-tree-dump-not "_gfortran_size" "original" } }
