! { dg-do run }
! 
! Coarray support
! PR fortran/18918

implicit none
integer :: n
character(len=30) :: str
critical
end critical
myCr: critical
end critical myCr

!
! Test SYNC ALL
!
sync all
sync all ( )
sync all (errmsg=str)

n = 5
sync all (stat=n)
if (n /= 0) STOP 1

n = 5
sync all (stat=n,errmsg=str)
if (n /= 0) STOP 2

!
! Test SYNC MEMORY
!
sync memory
sync memory ( )
sync memory (errmsg=str)

n = 5
sync memory (stat=n)
if (n /= 0) STOP 3

n = 5
sync memory (errmsg=str,stat=n)
if (n /= 0) STOP 4

!
! Test SYNC IMAGES
!
sync images (*)

if (this_image() == 1) then
    sync images (1)
    sync images (1, errmsg=str)
    sync images ([1])
end if

! Need to sync all here, because otherwise sync image 1 may overlap with the
! sync images(*, stat=n) below and that may hang for num_images() > 1.
sync all

n = 5
sync images (*, stat=n)
if (n /= 0) STOP 5

n = 5
sync images (*,errmsg=str,stat=n)
if (n /= 0) STOP 6

sync all
end
