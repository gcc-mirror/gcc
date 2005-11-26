! { dg-do run } 
! PR 24945
! Test reopening file without status specifier or with
! status='unknown'. The standard says that these two must behave
! identically, but the actual behaviour is processor dependent.
program open_status_2
  open(10, file="f", form='unformatted', status='unknown')
  open(10, file="f", form='unformatted', status='unknown')
  open(10, file="f", form='unformatted')
  close(10, status='delete')
end program open_status_2
 
