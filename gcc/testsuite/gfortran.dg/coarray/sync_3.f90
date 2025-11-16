! { dg-do run }
! { dg-options "-fcheck=all" }
! { dg-shouldfail "Invalid image number -1 in SYNC IMAGES" }
!
! As sync_1, but with bounds checking enabled.
! PR fortran/52161
!
! Coarray support
! PR fortran/18918

implicit none
integer :: n, st
integer,allocatable :: others(:)
character(len=40) :: str
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
sync images (*, errmsg=str, stat=n)
if (n /= 0) STOP 6

if (this_image() == num_images()) then
  others = (/( n, n=1, (num_images() - 1)) /)
  sync images(others)
else
  sync images ( num_images() )
end if 

n = -1
st = 0
sync images (n, errmsg=str, stat=st)
if (st /= 1 .OR. str /= "Invalid image number -1 in SYNC IMAGES") STOP 7

! Do this only on image 1, or output of error messages will clutter
if (this_image() == 1) sync images (n) ! Invalid: "-1"

end

! { dg-output "Fortran runtime error: Invalid image number -1 in SYNC IMAGES" }
