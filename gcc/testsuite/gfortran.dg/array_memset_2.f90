! { dg-do run }
! { dg-options "-O2 -fdump-tree-original" }

module foo
contains
  subroutine bar(a)
    real, dimension(:,:) :: a
    a(1,:) = 0.
  end subroutine bar
end module foo

program test
  use foo
  implicit none
  real, dimension (2,2) :: a, d, e
  real, dimension (1,2) :: b
  real, dimension (2) :: c
  data a, d, e /12*1.0/
  data b /2*1.0/
  data c /2*1.0/

  a(1,:) = 0.    ! This can't be optimized to a memset.
  b(1,:) = 0.    ! This is optimized to = {}.
  c = 0.         ! This is optimized to = {}.
  d(:,1) = 0.    ! This can't be otimized to a memset.
  call bar(e)

  if (any(a /= reshape((/ 0.0, 1.0, 0.0, 1.0/), shape(a)))) call abort
  if (any(b /= 0.)) call abort
  if (any(c /= 0.)) call abort
  if (any(d /= reshape((/ 0.0, 0.0, 1.0, 1.0/), shape(d)))) call abort
  if (any(e /= reshape((/ 0.0, 1.0, 0.0, 1.0/), shape(e)))) call abort

end program

! { dg-final { scan-tree-dump-times "= {}" 2 "original" } }
