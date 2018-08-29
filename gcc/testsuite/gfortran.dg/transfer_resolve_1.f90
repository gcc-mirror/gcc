! { dg-do run }
! PR40847 - an error in gfc_resolve_transfer caused the character length
! of 'mold' to be set incorrectly.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
program test_elemental

if (any (transfer_size((/0.,0./),(/'a','b'/)) .ne. [4 ,4])) STOP 1

contains

   elemental function transfer_size (source, mold)
     real, intent(in)         :: source
     character(*), intent(in) :: mold
     integer                  :: transfer_size
     transfer_size = SIZE(TRANSFER(source, (/mold/)))
     return
   end function transfer_size

end program test_elemental
