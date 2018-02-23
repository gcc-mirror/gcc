! { dg-do run }
!
! Check that the error is properly diagnosed and the strings are correctly padded.
!
integer, allocatable :: A, B(:)
integer :: stat
character(len=5) :: sstr
character(len=200) :: str

str = repeat('X', len(str))
deallocate(a, stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to deallocate an unallocated object") STOP 1

str = repeat('Y', len(str))
deallocate(b, stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to deallocate an unallocated object") STOP 2

sstr = repeat('Q', len(sstr))
deallocate(a, stat=stat, errmsg=sstr)
!print *, stat, trim(sstr)
if (stat == 0 .or. sstr /= "Attem") STOP 3

sstr = repeat('P', len(sstr))
deallocate(b, stat=stat, errmsg=sstr)
!print *, stat, trim(sstr)
if (stat == 0 .or. sstr /= "Attem") STOP 4

end
