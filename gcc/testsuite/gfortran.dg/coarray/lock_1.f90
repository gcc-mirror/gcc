! { dg-do run }
!
! LOCK/UNLOCK check
!
! PR fortran/18918
!
use iso_fortran_env
implicit none

type(lock_type) :: lock[*]
integer :: stat
integer :: counter
logical :: acquired

LOCK(lock)
UNLOCK(lock)

stat = 99
LOCK(lock, stat=stat)
if (stat /= 0) STOP 1

stat = 99
UNLOCK(lock, stat=stat)
if (stat /= 0) STOP 2

acquired = .false.
counter = 0
do while (.not. acquired)
  counter = counter + 1
  LOCK (lock[this_image()], acquired_lock=acquired)
  if (acquired) UNLOCK (lock)
  if ((num_images() .eq. 8) .and. (counter .gt. 100)) exit
end do
! counter here can vary depending on conditions, picked 100.
if (counter .gt. 100) stop counter
end
