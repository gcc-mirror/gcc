! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
!
! LOCK/UNLOCK intrinsics
!
! PR fortran/18918
!
integer :: a[*]
integer :: s
character(len=3) :: c
logical :: bool

LOCK (a, stat=s, acquired_lock=bool, errmsg=c) ! { dg-error "must be a scalar of type LOCK_TYPE" }
UNLOCK (a, stat=s, errmsg=c) ! { dg-error "must be a scalar of type LOCK_TYPE" }
end
