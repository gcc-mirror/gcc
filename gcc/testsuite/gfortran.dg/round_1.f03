! { dg-do run }
! PR35962 Implement F2003 rounding modes.
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
character(11) :: fmt(7)
character(80) :: line
integer :: i
fmt = (/'(RU,6F10.1)', '(RD,6F10.1)', '(RZ,6F10.1)', &
        '(RN,6F10.2)', '(RC,6F10.2)', '(RP,6F10.1)', &
        '(SP,6F10.1)' /)
do i = 1, 7
   !print fmt(i), 1.20, 1.22, 1.25, 1.27, 1.30, 1.125
end do
write(line, fmt(1)) 1.20, 1.22, 1.25, 1.27, 1.30, 1.125
if (line.ne."       1.3       1.3       1.3       1.3       1.3       1.2") STOP 1
write(line, fmt(2)) 1.20, 1.22, 1.25, 1.27, 1.30, 1.125
if (line.ne."       1.2       1.2       1.2       1.2       1.2       1.1") STOP 2
write(line, fmt(3)) 1.20, 1.22, 1.25, 1.27, 1.30, 1.125
if (line.ne."       1.2       1.2       1.2       1.2       1.2       1.1") STOP 3
write(line, fmt(4)) 1.20, 1.22, 1.25, 1.27, 1.30, 1.125
if (line.ne."      1.20      1.22      1.25      1.27      1.30      1.12") STOP 4
write(line, fmt(5)) 1.20, 1.22, 1.25, 1.27, 1.30, 1.125
if (line.ne."      1.20      1.22      1.25      1.27      1.30      1.13") STOP 5
write(line, fmt(6)) 1.20, 1.22, 1.250001, 1.27, 1.30, 1.125
if (line.ne."       1.2       1.2       1.3       1.3       1.3       1.1") STOP 6
write(line, fmt(7)) 1.20, 1.22, 1.250001, 1.27, 1.30, 1.125
if (line.ne."      +1.2      +1.2      +1.3      +1.3      +1.3      +1.1") STOP 7

end
