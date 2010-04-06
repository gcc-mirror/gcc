! { dg-do run }
! { dg-shouldfail "error stop" }
! 
! Coarray support
! PR fortran/18918

implicit none
integer :: n
character(len=30) :: str
critical
end critical
myCr: critical
end critical myCr
  sync all
  sync all ( )
  n = 5
  sync all (stat=n)
  if (n /= 0) call abort()
  n = 5
  sync all (stat=n,errmsg=str)
  if (n /= 0) call abort()
  sync all (errmsg=str)

  sync memory
  sync memory ( )
  n = 5
  sync memory (stat=n)
  if (n /= 0) call abort()
  n = 5
  sync memory (errmsg=str,stat=n)
  if (n /= 0) call abort()
  sync memory (errmsg=str)

sync images (*, stat=n)
sync images (1, errmsg=str)
sync images ([1],errmsg=str,stat=n)

sync images (*)
sync images (1)
sync images ([1])

if (num_images() /= 1) call abort()
error stop 'stop'
end

! { dg-output "ERROR STOP stop" }
