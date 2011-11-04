! { dg-do compile }
! { dg-options "-Warray-temporaries -O -fdump-tree-original" }
!
! PR fortran/43829
! Scalarization of reductions.
! Test that product is properly inlined.

! For more extended tests, see inline_sum_1.f90

  implicit none


  integer :: i

  integer, parameter :: q = 2
  integer, parameter :: nx=3, ny=2*q, nz=5
  integer, parameter, dimension(nx,ny,nz) :: p  = &
        & reshape ((/ (i, i=1,size(p)) /), shape(p))


  integer, dimension(nx,ny,nz) :: a
  integer, dimension(nx,   nz) :: ay

  a  = p

  ay = product(a,2)

end
! { dg-final { scan-tree-dump-times "struct array._integer\\(kind=4\\) atmp" 0 "original" } }
! { dg-final { scan-tree-dump-times "struct array\[^\\n\]*atmp" 0 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_product_" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
