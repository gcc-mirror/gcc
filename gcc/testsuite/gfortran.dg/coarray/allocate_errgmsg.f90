! { dg-do run }
!
! Check handling of errmsg.
!
implicit none
integer, allocatable :: a[:], b(:)[:], c, d(:)
integer :: stat
character(len=300) :: str

allocate(a[*], b(1)[*], c, d(2), stat=stat)

str = repeat('X', len(str))
allocate(a[*], stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to allocate an allocated object") &
  call abort ()

str = repeat('Y', len(str))
allocate(b(2)[*], stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to allocate an allocated object") &
  call abort ()

str = repeat('Q', len(str))
allocate(c, stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to allocate an allocated object") &
  call abort ()

str = repeat('P', len(str))
allocate(d(3), stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to allocate an allocated object") &
  call abort ()

end
