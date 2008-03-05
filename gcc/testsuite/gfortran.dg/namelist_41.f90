! { dg-do run { target fd_truncate } }
! PR34291 Segfault on &end in namelist expanded read of character
  implicit none
  character(len=10), dimension(2)  :: var
  namelist /inx/ var
  var = "goodbye"
  open(unit=11, status='scratch')
  write (11, *) "&inx"
  write (11, *) "var(1)='hello'" 
  write (11, *) "&end"
  rewind (11)
  read(11,nml=inx)
  if (var(1) /= 'hello' .and. var(2) /= 'goodbye') call abort
  var = "goodbye"
  rewind (11)
  write (11, *) "$inx"
  write (11, *) "var(1)='hello'" 
  write (11, *) "$end"
  rewind (11)
  read(11,nml=inx)
  if (var(1) /= 'hello' .and. var(2) /= 'goodbye') call abort
end
