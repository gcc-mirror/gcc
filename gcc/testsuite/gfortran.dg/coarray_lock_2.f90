! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2003" }
!
! LOCK/UNLOCK intrinsics
!
! PR fortran/18918
!
integer :: a[*] ! { dg-error "Fortran 2008: Coarray declaration" }
integer :: s
character(len=3) :: c
logical :: bool

LOCK (a, stat=s, acquired_lock=bool, errmsg=c) ! { dg-error "Fortran 2008: LOCK statement" }
UNLOCK (a, stat=s, errmsg=c) ! { dg-error "Fortran 2008: UNLOCK statement" }
end
