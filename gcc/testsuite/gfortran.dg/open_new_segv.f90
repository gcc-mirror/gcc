! { dg-do run }
! PR 64770 SIGSEGV when trying to open an existing file with status="new"
program pr64770
  implicit none
  ! Make sure pr64770test.dat exists
  open(99, file="pr64770test.dat", status="replace")
  close(99)
  open(99, file="pr64770test.dat", access="stream", form="unformatted", &
       status="new")
end program pr64770
