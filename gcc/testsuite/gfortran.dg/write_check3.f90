! { dg-do run }
! PR29936 Missed constraint on RECL=specifier in unformatted sequential WRITE
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program us_recl
  real, dimension(5) :: array = 5.4321
  integer :: istatus
  open(unit=10, form="unformatted", access="sequential", RECL=16)
  write(10, iostat=istatus) array
  if (istatus == 0) call abort()
  close(10, status="delete")
end program us_recl
