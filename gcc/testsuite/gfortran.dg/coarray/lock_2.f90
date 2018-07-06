! { dg-do run }
!
! LOCK/UNLOCK check
!
! PR fortran/18918
!

use iso_fortran_env
implicit none

type(lock_type), allocatable :: lock1[:]
type(lock_type), allocatable :: lock2(:,:)[:]
type(lock_type) :: lock3(4)[*]
integer :: stat
logical :: acquired

allocate(lock1[*])
allocate(lock2(2,2)[*])

LOCK(lock1)
UNLOCK(lock1)

LOCK(lock2(1,1))
LOCK(lock2(2,2))
UNLOCK(lock2(1,1))
UNLOCK(lock2(2,2))

LOCK(lock3(3))
LOCK(lock3(4))
UNLOCK(lock3(3))
UNLOCK(lock3(4))

stat = 99
LOCK(lock1, stat=stat)
if (stat /= 0) STOP 1

LOCK(lock2(1,1), stat=stat)
if (stat /= 0) STOP 2
LOCK(lock2(2,2), stat=stat)
if (stat /= 0) STOP 3

LOCK(lock3(3), stat=stat)
if (stat /= 0) STOP 4
LOCK(lock3(4), stat=stat)
if (stat /= 0) STOP 5

stat = 99
UNLOCK(lock1, stat=stat)
if (stat /= 0) STOP 6

UNLOCK(lock2(1,1), stat=stat)
if (stat /= 0) STOP 7
UNLOCK(lock2(2,2), stat=stat)
if (stat /= 0) STOP 8

UNLOCK(lock3(3), stat=stat)
if (stat /= 0) STOP 9
UNLOCK(lock3(4), stat=stat)
if (stat /= 0) STOP 10

if (this_image() == 1) then
  acquired = .false.
  LOCK (lock1[this_image()], acquired_lock=acquired)
  if (.not. acquired) STOP 11

  acquired = .false.
  LOCK (lock2(1,1)[this_image()], acquired_lock=acquired)
  if (.not. acquired) STOP 12

  acquired = .false.
  LOCK (lock2(2,2)[this_image()], acquired_lock=acquired)
  if (.not. acquired) STOP 13

  acquired = .false.
  LOCK (lock3(3)[this_image()], acquired_lock=acquired)
  if (.not. acquired) STOP 14

  acquired = .false.
  LOCK (lock3(4)[this_image()], acquired_lock=acquired)
  if (.not. acquired) STOP 15

  UNLOCK (lock1[1])
  UNLOCK (lock2(1,1)[1])
  UNLOCK (lock2(2,2)[1])
  UNLOCK (lock3(3)[1])
  UNLOCK (lock3(4)[1])
end if
end

