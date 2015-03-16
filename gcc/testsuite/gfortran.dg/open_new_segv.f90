! { dg-do run }
! { dg-shouldfail "Cannot open file" }
! PR 64770 SIGSEGV when trying to open an existing file with status="new"
program pr64770
  implicit none
  ! Make sure pr64770test.dat exists
  open(99, file="pr64770test.dat", status="replace")
  close(99)
  open(99, file="pr64770test.dat", access="stream", form="unformatted", &
       status="new")
end program pr64770
! { dg-output "At line 10 of file.*" }
! { dg-output "Fortran runtime error: Cannot open file .pr64770test.dat.:" }
! { dg-final { remote_file build delete "pr64770test.dat" } }
