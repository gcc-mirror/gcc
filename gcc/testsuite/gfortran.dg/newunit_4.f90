! { dg-do compile }
! PR69110 ICE with NEWUNIT 
subroutine open_file_safe(fname, fstatus, faction, fposition, funit)
  character(*), intent(in)  :: fname, fstatus, faction, fposition
  integer, intent(out)      :: funit
  open(newunit=funit, status=fstatus)
end subroutine open_file_safe
