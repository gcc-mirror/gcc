! { dg-do run }
! Contributed by Dominique Dhumieres <dominiq@lps.ens.fr>

character(15) :: str="+ .339  567+2"
real, parameter :: should_be = .339567e2
real, parameter :: eps = 10 * epsilon (should_be)
real :: x, y

read(str,'(BN,F15.6)') x
print *, x
read(str,'(G15.7)') y
print *, y

if (abs (x - should_be) > eps .or. abs (y - should_be) > eps) then
  STOP 1
end if

end
