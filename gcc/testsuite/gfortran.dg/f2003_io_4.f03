! { dg-do run { target fd_truncate } }
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! Test of decimal= feature

integer :: istat
character(80) :: msg
real, dimension(4) :: a, b, c
namelist /mynml/ a, b
msg = "yes"
a = 43.21
b = 3.131
c = 5.432
open(99, decimal="comma")
write(99,'(10f8.3)') a
a = 0.0
rewind(99)
read(99,'(10f8.3)') a
if (any(a.ne.43.21)) call abort

write(msg,'(dp,f8.3,dc,f8.2,dp,f8.3)', decimal="comma") a(1), b(1), c(1)
if (trim(msg).ne."  43.210    3,13   5.432") call abort

close(99, status="delete")
open(99, decimal="comma")
write(99,nml=mynml)
a = 0.0
b = 0.0
rewind(99)
read(99,nml=mynml)
if (any(a.ne.43.21)) call abort
if (any(b.ne.3.131)) call abort
close(99, status="delete")
end
