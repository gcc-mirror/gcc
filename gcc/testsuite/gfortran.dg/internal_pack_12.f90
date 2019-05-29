! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! Test the fix for PR43243, where unnecessary calls to internal_pack/unpack
! were being produced below. These references are contiguous and so do not
! need a temporary. In addition, the final call to 'bar' required a pack/unpack
! which had been missing since r156680, at least.
!
! Contributed Tobias Burnus <burnus@gcc.gnu.org>
!
module m
  type t
    integer, allocatable :: a(:)
    integer, pointer :: b(:)
    integer :: c(5)
  end type t
end module m

subroutine foo(a,d,e,n)
  use m
  implicit none
  integer :: n
  type(t) :: a
  type(t), allocatable :: d(:)
  type(t), pointer :: e(:)
  call bar(   a%a) ! OK - no array temp needed
  call bar(   a%c) ! OK - no array temp needed

  call bar(   a%a(1:n)) ! Missed: No pack needed
  call bar(   a%b(1:n)) ! OK: pack needed
  call bar(   a%c(1:n)) ! Missed: No pack needed

  call bar(d(1)%a(1:n)) ! Missed: No pack needed
  call bar(d(1)%b(1:n)) ! OK: pack needed
  call bar(d(1)%c(1:n)) ! Missed: No pack needed

  call bar(e(1)%a(1:n)) ! Missed: No pack needed
  call bar(e(1)%b(1:n)) ! OK: pack needed
  call bar(e(1)%c(1:n)) ! Missed: No pack needed
end subroutine foo

use m
implicit none
integer :: i
integer, target :: z(6)
type(t) :: y

z = [(i, i=1,6)]
y%b => z(::2)
call bar(y%b)  ! Missed: Pack needed
end

subroutine bar(x)
  integer :: x(1:*)
  print *, x(1:3)
  if (any (x(1:3) /= [1,3,5])) STOP 1
end subroutine bar
! { dg-final { scan-tree-dump-times "unpack" 4 "original" } }
