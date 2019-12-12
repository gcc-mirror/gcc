! { dg-do run }
! PR83560 list-directed formatting of INTEGER is missing plus on output
! when output open with SIGN='PLUS'
character(64) :: astring
i=789
open(unit=10, status='scratch', sign='plus')
write(10,*) i
rewind(10)
read(10,*) astring
close (10)
if (astring.ne.'+789') STOP 1
end
