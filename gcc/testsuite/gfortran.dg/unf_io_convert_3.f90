! { dg-do run }
! { dg-require-effective-target fortran_large_real }
program main
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(kind=k) a,b,c
  a = 1.1_k
  open(10,convert="swap",form="unformatted") ! { dg-warning "Extension: CONVERT" }
  write(10) a
  backspace 10
  read (10) b
  close(10,status="delete")
  if (a /= b) STOP 1
  write (11) a
  backspace 11
  open (11,form="unformatted")
  read (11) c
  if (a .ne. c) STOP 2
  close (11, status="delete")
end program main
