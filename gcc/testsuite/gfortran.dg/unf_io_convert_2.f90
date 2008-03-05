! { dg-do run { target fd_truncate } }
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
  if (a(1) /= 3.14 .or. a(2) /= 2.71) call abort
  close(10,status="delete")

  open (10, form="unformatted",convert="big_endian") ! { dg-warning "Extension: CONVERT" }
  i = (/ Z'11223344', Z'55667700' /)
  write (10) i
  rewind (10)
  read (10) b
  if (any(b /= (/ Z'11', Z'22', Z'33', Z'44', Z'55', Z'66', Z'77', Z'00' /))) &
    call abort
  backspace 10
  read (10) j
  if (j /= Z'1122334455667700') call abort
  close (10, status="delete")

  open (10, form="unformatted", convert="little_endian") ! { dg-warning "Extension: CONVERT" }
  write (10) i
  rewind (10)
  read (10) b
  if (any(b /= (/ Z'44', Z'33', Z'22', Z'11', Z'00', Z'77', Z'66', Z'55' /))) &
    call abort
  backspace 10
  read (10) j
  if (j /= Z'5566770011223344') call abort
  close (10, status="delete")

end program main
