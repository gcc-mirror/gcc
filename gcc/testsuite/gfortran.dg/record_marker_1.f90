! { dg-do run }
! { dg-options "-frecord-marker=4" }

program main
  implicit none
  integer(kind=4) :: i1, i2, i3

  open(15,form="UNFORMATTED")
  write (15) 1_4
  close (15)
  open (15,form="UNFORMATTED",access="DIRECT",recl=4)
  i1 = 1_4
  i2 = 2_4
  i3 = 3_4
  read (15,rec=1) i1
  read (15,rec=2) i2
  read (15,rec=3) i3
  close (15, status="DELETE")
  if (i1 /= 4_4) STOP 1
  if (i2 /= 1_4) STOP 2
  if (i3 /= 4_4) STOP 3

  open(15,form="UNFORMATTED",convert="SWAP")
  write (15) 1_4
  close (15)
  open (15,form="UNFORMATTED",access="DIRECT",convert="SWAP",recl=4)
  i1 = 1_4
  i2 = 2_4
  i3 = 3_4
  read (15,rec=1) i1
  read (15,rec=2) i2
  read (15,rec=3) i3
  close(15,status="DELETE")
  if (i1 /= 4_4) STOP 4
  if (i2 /= 1_4) STOP 5
  if (i3 /= 4_4) STOP 6

end program main
