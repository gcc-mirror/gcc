! { dg-do run }
! PR31201 Too large unit number generates wrong code
! This tests initialization of the IOSTAT variable
  integer :: i
  character(len=50) :: str
  write (2_8*int(huge(0_4),kind=8)+9_8, iostat=i, iomsg=str) 555
  if (i.ne.5005) call abort
  if (str.ne."Unit number in I/O statement too large") call abort
  end 