! { dg-do run }
program main
  complex(kind=4) :: c
  real(kind=4) :: a(2)
  integer(kind=4) :: i(2)
  integer(kind=1) :: b(8)
  integer(kind=8) :: j

  c = (3.14, 2.71)
  open (10, form="unformatted",convert="swap") ! { dg-warning "Extension: CONVERT" }
  write (10) c
  rewind (10)
  read (10) a
  if (a(1) /= 3.14 .or. a(2) /= 2.71) STOP 1
  close(10,status="delete")

  open (10, form="unformatted",convert="big_endian") ! { dg-warning "Extension: CONVERT" }
  i = (/ int(Z'11223344'), int(Z'55667700') /)
  write (10) i
  rewind (10)
  read (10) b
  if (any(b /= (/ int(Z'11',1), int(Z'22',1), int(Z'33',1), int(Z'44',1), &
  &   int(Z'55',1), int(Z'66',1), int(Z'77',1), int(Z'00',1) /))) &
    STOP 2
  backspace 10
  read (10) j
  if (j /= int(Z'1122334455667700',8)) STOP 3
  close (10, status="delete")

  open (10, form="unformatted", convert="little_endian") ! { dg-warning "Extension: CONVERT" }
  write (10) i
  rewind (10)
  read (10) b
  if (any(b /= (/ int(Z'44',1), int(Z'33',1), int(Z'22',1), int(Z'11',1), &
  &   int(Z'00',1),  int(Z'77',1), int(Z'66',1), int(Z'55',1) /))) &
    STOP 4
  backspace 10
  read (10) j
  if (j /= int(Z'5566770011223344',8)) STOP 5
  close (10, status="delete")

end program main
