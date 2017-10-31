! { dg-do  run }
! PR 82233 - there were program aborts for some of these commands.
! Original test case by Urban Jost.
program boom
implicit none
integer                       :: i,j 
character(len=256)            :: msg
character(len=:), allocatable :: command
   command='notthere'
   msg='' ! seems to only be defined if exitstatus.ne.0
   ! ok -- these work
   call execute_command_line(command , wait=.false., exitstat=i, cmdstat=j, cmdmsg=msg)
   if (j /= 0 .or. msg /= '') call abort
   call execute_command_line(command ,               exitstat=i, cmdstat=j, cmdmsg=msg )
   if (j /= 3 .or. msg /= "Invalid command line" ) call abort
   msg = ''
   call execute_command_line(command , wait=.false., exitstat=i,            cmdmsg=msg )
   if (j /= 3) call abort
   call execute_command_line(command , wait=.false., exitstat=i                        )
   if (msg /= '') call abort
   call execute_command_line(command ,               exitstat=i, cmdstat=j             )

end program boom
