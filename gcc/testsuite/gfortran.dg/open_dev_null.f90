! { dg-do run }
! PR45723 opening /dev/null for appending writes fails
logical :: thefile
inquire(file="/dev/null",exist=thefile)
if (thefile) then
  open(unit=7,file="/dev/null",position="append")
  close(7)
endif
end
