! { dg-do run }
!
! Check the fix for PR34955 in which three bytes would be copied
! from bytes by TRANSFER, instead of the required two and the
! resulting string length would be incorrect.
!
! Contributed by Dominique Dhumieres  <dominiq@lps.ens.fr>
!
  character(len = 1)  :: string = "z"
  character(len = 20) :: tmp = ""
  tmp = Upper ("abcdefgh")
  if (trim(tmp) .ne. "ab") call abort ()
contains
  Character (len = 20) Function Upper (string)
    Character(len = *) string
    integer :: ij
    i = size (transfer (string,"xy",len (string)))
    if (i /= len (string)) call abort ()
    Upper = ""
    Upper(1:2) = &
    transfer (merge (transfer (string,"xy",len (string)),    &
      string(1:2), .true.), "xy")
    return
  end function Upper
end
