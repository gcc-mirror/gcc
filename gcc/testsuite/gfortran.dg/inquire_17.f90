! { dg-do run }
! PR77707 formatted direct access: nextrec off by one
program directaccess_formatted
  integer nextrec
  open(10, status='scratch', form='formatted', access='direct', recl=10*4)
  write(10,'(10i4)',rec=9) 1,2,3,4,5,6,7,8,9,10
  inquire(unit=10,nextrec=nextrec)
  if (nextrec.ne.10) STOP 1
  close(10)
end
