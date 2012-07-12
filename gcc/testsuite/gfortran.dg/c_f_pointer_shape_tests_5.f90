! { dg-do run }
!
! Check that C_F_Pointer works with a noncontiguous SHAPE argument
!
use iso_c_binding
type(c_ptr) :: x
integer, target :: array(3)
integer, pointer :: ptr(:,:)
integer, pointer :: ptr2(:,:,:)
integer :: myshape(5)

array = [22,33,44]
x = c_loc(array)
myshape = [1,2,3,4,1]

call c_f_pointer(x, ptr, shape=myshape(1:4:2))
if (any (lbound(ptr) /= [ 1, 1])) call abort ()
if (any (ubound(ptr) /= [ 1, 3])) call abort ()
if (any (shape(ptr) /= [ 1, 3])) call abort ()
if (any (ptr(1,:) /= array)) call abort()

call c_f_pointer(x, ptr2, shape=myshape([1,3,1]))
if (any (lbound(ptr2) /= [ 1, 1, 1])) call abort ()
if (any (ubound(ptr2) /= [ 1, 3, 1])) call abort ()
if (any (shape(ptr2) /= [ 1, 3, 1])) call abort ()
if (any (ptr2(1,:,1) /= array)) call abort()
end
