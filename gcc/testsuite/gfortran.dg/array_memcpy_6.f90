! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
module x
  implicit none
  private
  integer :: i, j
  integer, parameter :: n = 6, m = 4
  integer(kind=1), dimension(n, m), parameter :: fld = reshape ([ &
       ((i*j,i=1,n),j=1,n)], &
       shape = [n,m])
  public :: fld,n,m
end module x

program memain
  use x
  implicit none
  integer(kind=1), dimension(n) :: buf
  integer :: i, j
  do j=1,m
     buf = fld(:,j)
     do i=1,n
        if (buf(i) /= fld(i,j)) stop 1
     end do
  end do
end program memain
! { dg-final { scan-tree-dump-times "__builtin_memcpy" 1 "original" } }
