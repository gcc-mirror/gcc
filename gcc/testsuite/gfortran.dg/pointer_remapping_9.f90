! { dg-do run }
!
! PR fortran/61138
! Wrong code with pointer-bounds remapping
!
! Contributed by Tobias Burnus <burnus@net-b.de>

implicit none
integer, target :: tgt(10)
integer, target, allocatable :: tgt2(:)
integer, pointer :: ptr(:)

tgt = [1,2,3,4,5,6,7,8,9,10]
tgt2 = [1,2,3,4,5,6,7,8,9,10]


ptr(-5:) => tgt(5:)  ! Okay

if (size(ptr) /= 6 .or. lbound(ptr,1) /= -5) call abort()
if (any (ptr /= [5,6,7,8,9,10])) call abort()


ptr(-5:) => tgt2(5:)  ! wrongly associates the whole array

print '(*(i4))', size(ptr), lbound(ptr)
print '(*(i4))', ptr

if (size(ptr) /= 6 .or. lbound(ptr,1) /= -5) call abort()
if (any (ptr /= [5,6,7,8,9,10])) call abort()
end

