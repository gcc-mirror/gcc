! { dg-do compile }
!
! PR fortran/51302
!
! Volatile DO variable - was ICEing before
!
integer, volatile :: i
integer :: n = 1
do i = 1, n
end do
end
