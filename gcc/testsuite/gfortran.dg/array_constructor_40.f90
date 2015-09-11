! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 55806 - replace ANY intrinsic for array
! constructor with .or.

module mymod
  implicit none
contains
  subroutine bar(a,b,c, lo)
    real, dimension(3,3), intent(in) :: a,b
    logical, dimension(3,3), intent(in) :: lo
    integer, intent(out) :: c
    real, parameter :: acc = 1e-4
    integer :: i,j
    
    c = 0
    do i=1,3
       if (any([abs(a(i,1) - b(i,1)) > acc,  &
            (j==i+1,j=3,8)])) cycle
       if (any([abs(a(i,2) - b(i,2)) > acc, &
            abs(a(i,3) - b(i,3)) > acc, lo(i,:)])) cycle
       c = c + i
    end do
  end subroutine bar

  subroutine baz(a, b, c)
    real, dimension(3,3), intent(in) :: a,b
    real, intent(out) :: c
    c = sum([a(1,1),a(2,2),a(3,3),b(:,1)])
  end subroutine baz
end module mymod

program main
  use mymod
  implicit none
  real, dimension(3,3) :: a,b
  real :: res
  integer :: c
  logical lo(3,3)
  data a/1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9/

  b = a
  b(2,2) = a(2,2) + 0.2
  lo = .false.
  lo(3,3) = .true.
  call bar(a,b,c,lo)
  if (c /= 1) call abort
  call baz(a,b,res);
  if (abs(res - 8.1) > 1e-5) call abort
end program main
! { dg-final { scan-tree-dump-times "while" 5 "original" } }
