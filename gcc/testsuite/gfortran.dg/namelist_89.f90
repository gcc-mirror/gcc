! { dg-do run }
! PR69456 Namelist value with trailing sign is ignored without error
implicit none
integer :: ios
character(256) :: errormsg
real :: r1 = -1
real :: r2 = -1
real :: r3 = -1
real :: r4 = -1
complex :: c1 = (-1,-1)
namelist /nml/ r1, r2, r3, r4, c1

open (99, status="scratch")

write(99,*) "&nml"
write(99,*) "  r1=1+1"      ! Treated as 1e+1!
write(99,*) "  r2=1-1"      ! Treated as 1e-1!
write(99,*) "  r3=1+1"      ! Treated as 1e+1!
write(99,*) "  r4=1-1"      ! Treated as 1e-1!
write(99,*) "  c1=(1-,1+1)" ! Should give error on item number 5
write(99,*) "/"

rewind(99)

read (99, nml=nml, iostat=ios, iomsg=errormsg)
if (ios.ne.5010) call abort
if (scan(errormsg, "5").ne.44) call abort

rewind(99)

write(99,*) "&nml"
write(99,*) "  r1=1+1"       ! Treated as 1e+1!
write(99,*) "  r2=1-"        ! Should give error on item number 2
write(99,*) "  r3=1+1"       ! Treated as 1e+1!
write(99,*) "  r4=1-1"       ! Treated as 1e-1!
write(99,*) "  c1=(1-1,1+1)" ! Treated as (1e-1,1e+1)!
write(99,*) "/"

rewind(99)

read (99, nml=nml, iostat=ios, iomsg=errormsg)
if (ios.ne.5010) call abort
if (scan(errormsg, "2").ne.25) call abort

close (99)

end
