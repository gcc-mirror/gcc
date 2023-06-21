! { dg-do run }
! { dg-options "-std=f2003" }
! PR109662-a semi-colon after namelist name accepted on input. 
program testnmlread
  implicit none
  character(16) :: line = '&stuff; n = 759/'
  character(100)::message
  integer       :: n, i, ioresult
  namelist/stuff/n
  message = ""
  ioresult = 0
  n = 99
  read(line,nml=stuff,iostat=ioresult)
  if (ioresult == 0) STOP 13 ! Should error with the semi-colon in there.

  ! Intentional short input (-> EOF)
  line = "&stuff"
  ! Problem manifests on two bad reads on same string.
  do i = 1, 6
    n = -1
    ioresult = 0

    read (line,nml=stuff,iostat=ioresult)
    if (n /= -1) STOP 24
    if (ioresult == 0) STOP 25
  end do

end program testnmlread
