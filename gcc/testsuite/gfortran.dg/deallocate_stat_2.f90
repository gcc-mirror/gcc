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
if (stat == 0 .or. str /= "Attempt to deallocate an unallocated object") call abort()

str = repeat('Y', len(str))
deallocate(b, stat=stat, errmsg=str)
!print *, stat, trim(str)
if (stat == 0 .or. str /= "Attempt to deallocate an unallocated object") call abort()

sstr = repeat('Q', len(sstr))
deallocate(a, stat=stat, errmsg=sstr)
!print *, stat, trim(sstr)
if (stat == 0 .or. sstr /= "Attem") call abort()

sstr = repeat('P', len(sstr))
deallocate(b, stat=stat, errmsg=sstr)
!print *, stat, trim(sstr)
if (stat == 0 .or. sstr /= "Attem") call abort()

end
