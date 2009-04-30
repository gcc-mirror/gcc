! { dg-do run }
! pr 19467
! test namelist with character arrays
! Based on example provided by paulthomas2@wanadoo.fr

program pr19467
  implicit none
  integer             :: ier
  character(len=2)    :: ch(2)
  character(len=2)    :: dh(2)=(/"aa","bb"/)
  namelist /a/ ch
  open (10, status = "scratch")
  write (10, *) "&A ch = 'aa' , 'bb' /"
  rewind (10)
  READ (10,nml=a, iostat = ier)
  close (10)
  if ((ier /= 0) .or. (any (ch /= dh))) call abort ()
end program pr19467
