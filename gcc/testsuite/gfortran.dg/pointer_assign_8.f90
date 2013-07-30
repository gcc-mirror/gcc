! { dg-do run }
!
! PR fortran/57530
!
!
! TYPE => CLASS pointer assignment for variables
!
module m
  implicit none
  type t
    integer :: ii = 55
  end type t
contains
  subroutine sub (tgt, tgt2)
    class(t), target :: tgt, tgt2(:)
    type(t), pointer :: ptr, ptr2(:), ptr3(:,:)

    if (tgt%ii /= 43) call abort()
    if (size (tgt2) /= 3) call abort()
    if (any (tgt2(:)%ii /= [11,22,33])) call abort()

    ptr => tgt  ! TYPE => CLASS
    ptr2 => tgt2  ! TYPE => CLASS
    ptr3(-3:-3,1:3) => tgt2  ! TYPE => CLASS

    if (.not. associated(ptr)) call abort()
    if (.not. associated(ptr2)) call abort()
    if (.not. associated(ptr3)) call abort()
    if (.not. associated(ptr,tgt)) call abort()
    if (.not. associated(ptr2,tgt2)) call abort()
    if (ptr%ii /= 43) call abort()
    if (size (ptr2) /= 3) call abort()
    if (size (ptr3) /= 3) call abort()
    if (any (ptr2(:)%ii /= [11,22,33])) call abort()
    if (any (shape (ptr3) /= [1,3])) call abort()
    if (any (ptr3(-3,:)%ii /= [11,22,33])) call abort()
  end subroutine sub
end module m

use m
type(t), target :: x
type(t), target :: y(3)
x%ii = 43
y(:)%ii = [11,22,33]
call sub(x,y)
end
