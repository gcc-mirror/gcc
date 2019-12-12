! { dg-do run }
!
! Test the fix for PR70149 in which the string length for
! 'number_string' was not initialized.
!
! Contributed by Walter Spector  <w6ws@earthlink.net>
!
module myptr_mod
  implicit none

  integer, target, save :: int_data = 42
  character(16), target, save :: char_data = 'forty two'

  integer, pointer :: number => int_data
  character(:), pointer :: number_string => char_data

end module

  use myptr_mod
  if (LEN (number_string) .ne. 16) stop 1
  if (trim (number_string) .ne. 'forty two') stop 2
end

