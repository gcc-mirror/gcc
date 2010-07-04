! { dg-do compile }
! { dg-options "-fcoarray=single -fdump-tree-original" }
!
! PR fortran/18918
! PR fortran/43919 for boundsTest()
!
! Coarray intrinsics
!

subroutine image_idx_test1()
  INTEGER,save :: array[2,-1:4,8,*]
  WRITE (*,*) IMAGE_INDEX (array, [2,0,3,1])
  if (IMAGE_INDEX (array, [1,-1,1,1]) /= 1) call not_existing()
  if (IMAGE_INDEX (array, [2,-1,1,1]) /= 0) call not_existing()
  if (IMAGE_INDEX (array, [1,-1,1,2]) /= 0) call not_existing()
end subroutine

subroutine this_image_check()
  integer,save :: a(1,2,3,5)[0:3,*]
  integer :: j
  if (this_image() /= 1) call not_existing()
  if (this_image(a,dim=1) /= 0) call not_existing()
  if (this_image(a,dim=2) /= 1) call not_existing()
end subroutine this_image_check

subroutine othercheck()
real,save :: a(5)[2,*]
complex,save :: c[4:5,6,9:*]
integer,save :: i, j[*]
dimension :: b(3)
codimension :: b[5:*]
dimension :: h(9:10)
codimension :: h[8:*]
save :: b,h
if (this_image() /= 1) call not_existing()
if (num_images() /= 1) call not_existing()
if(any(this_image(coarray=a) /= [ 1, 1 ])) call not_existing()
if(any(this_image(c) /= [4,1,9])) call not_existing()
if(this_image(c, dim=3) /= 9) call not_existing()
if(ubound(b,dim=1) /= 3 .or. this_image(coarray=b,dim=1) /= 5) call not_existing()
if(ubound(h,dim=1) /= 10 .or. this_image(h,dim=1) /= 8) call not_existing()
end subroutine othercheck

subroutine andanother()
integer,save :: a(1)[2:9,4,-3:5,0:*]
print *, lcobound(a)
print *, lcobound(a,dim=3,kind=8)
print *, ucobound(a)
print *, ucobound(a,dim=1,kind=2)
if (any(lcobound(a) /= [2, 1, -3, 0])) call not_existing()
if (any(ucobound(a) /= [9, 4,  5, 0])) call not_existing()
if (lcobound(a,dim=3,kind=8) /= -3_8)  call not_existing()
if (ucobound(a,dim=1,kind=2) /=  9_2)  call not_existing()
end subroutine andanother

subroutine boundsTest()
  implicit none
  integer :: a[*] = 7
  if (any (lcobound(a) /= [1])) call not_existing()
  if (any (ucobound(a) /= [1])) call not_existing()
end subroutine boundsTest

! { dg-final { scan-tree-dump-times "not_existing" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
