! { dg-do run }
!
! PR 55072: [4.6/4.7/4.8 Regression] Missing internal_pack leads to wrong code with derived type
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

implicit none
type t
integer :: i
end type t
type(t), target :: tgt(4,4)
type(t), pointer :: p(:,:)
integer :: i,j,k

k = 1
do i = 1, 4
  do j = 1, 4
    tgt(i,j)%i = k
    k = k+1
  end do
end do

p => tgt(::2,::2)
print *,p%i
call bar(p)

contains

  subroutine bar(x)
    type(t) :: x(*)
    print *,x(1:4)%i
    if (any (x(1:4)%i /= [1, 9, 3, 11])) call abort()
  end subroutine
end
