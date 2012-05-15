! { dg-do run }
! Tests the fix for PR31867, in which the interface evaluation
! of the character length of 'join' (ie. the length available in
! the caller) was wrong.
!
! Contributed by <beliavsky@aol.com> 
!
module util_mod
  implicit none
contains
  function join (words, sep) result(str)
    character (len=*), intent(in)        :: words(:),sep
    character (len = (size (words) - 1) * len_trim (sep) + & 
               sum (len_trim (words)))   :: str
    integer                              :: i,nw
    nw  = size (words)
    str = ""
    if (nw < 1) then
      return
    else
      str = words(1)
    end if
    do i=2,nw
      str = trim (str) // trim (sep) // words(i)
    end do
  end function join
end module util_mod
!
program xjoin
  use util_mod, only: join
  implicit none
  integer yy
  character (len=5) :: words(5:8) = (/"two  ","three","four ","five "/), sep = "^#^"
  character (len=5) :: words2(4) = (/"bat  ","ball ","goal ","stump"/), sep2 = "&"

  if (join (words, sep) .ne. "two^#^three^#^four^#^five") call abort ()
  if (len (join (words, sep)) .ne. 25) call abort ()

  if (join (words(5:6), sep) .ne. "two^#^three") call abort ()
  if (len (join (words(5:6), sep)) .ne. 11) call abort ()

  if (join (words(7:8), sep) .ne. "four^#^five") call abort ()
  if (len (join (words(7:8), sep)) .ne. 11) call abort ()

  if (join (words(5:7:2), sep) .ne. "two^#^four") call abort ()
  if (len (join (words(5:7:2), sep)) .ne. 10) call abort ()

  if (join (words(6:8:2), sep) .ne. "three^#^five") call abort ()
  if (len (join (words(6:8:2), sep)) .ne. 12) call abort ()

  if (join (words2, sep2) .ne. "bat&ball&goal&stump") call abort ()
  if (len (join (words2, sep2)) .ne. 19) call abort ()

  if (join (words2(1:2), sep2) .ne. "bat&ball") call abort ()
  if (len (join (words2(1:2), sep2)) .ne. 8) call abort ()

  if (join (words2(2:4:2), sep2) .ne. "ball&stump") call abort ()
  if (len (join (words2(2:4:2), sep2)) .ne. 10) call abort ()

end program xjoin
