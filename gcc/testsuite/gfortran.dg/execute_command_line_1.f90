! { dg-do compile }
!
! Check that we accept all variants of the EXECUTE_COMMAND_LINE intrinsic.
!
  integer :: i, j
  character(len=100) :: s

  s = ""

  call execute_command_line ("ls *.f90")

  print *, "-----------------------------"

  call execute_command_line ("sleep 1 ; ls *.f90", .false.)
  print *, "I'm not waiting"
  call sleep(2)

  print *, "-----------------------------"

  call execute_command_line ("sleep 1 ; ls *.f90", .true.)
  print *, "I did wait"
  call sleep(2)

  print *, "-----------------------------"

  call execute_command_line ("ls *.f90", .true., i)
  print *, "Exist status was: ", i

  print *, "-----------------------------"

  call execute_command_line ("ls *.doesnotexist", .true., i)
  print *, "Exist status was: ", i

  print *, "-----------------------------"

  call execute_command_line ("echo foo", .true., i, j)
  print *, "Exist status was: ", i
  print *, "Command status was: ", j

  print *, "-----------------------------"

  call execute_command_line ("echo foo", .true., i, j, s)
  print *, "Exist status was: ", i
  print *, "Command status was: ", j
  print *, "Error message is: ", trim(s)

  print *, "-----------------------------"

  call execute_command_line ("ls *.doesnotexist", .true., i, j, s)
  print *, "Exist status was: ", i
  print *, "Command status was: ", j
  print *, "Error message is: ", trim(s)

  print *, "-----------------------------"

  call execute_command_line ("sleep 20", .false.)
  print *, "Please kill me with ^C"
  call sleep (10)

  end
