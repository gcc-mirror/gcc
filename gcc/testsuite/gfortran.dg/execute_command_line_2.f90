! { dg-do run }
!
! Check that EXECUTE_COMMAND_LINE handles invalid command lines appropriately
!
  integer :: s = 0, c = 0
  character(len=255) :: msg = ""

  ! This should fail, set CMDSTAT to nonzero value, and an error message
  ! in CMDMSG.
  call execute_command_line ("/nosuchfile", exitstat=s, cmdstat=c, cmdmsg=msg)
  if (c == 0) STOP 1
  if (len_trim(msg) == 0) STOP 2

end
