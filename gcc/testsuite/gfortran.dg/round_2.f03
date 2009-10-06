! { dg-do run }
! PR35962 Implement F2003 rounding modes.
! Test case prepared by Jerry Delisle  <jvdelisle@gcc.gnu.org>
character(64) :: line
write(line, '(RN, 4F10.3)') 0.0625_10, 0.1875_10
if (line.ne."     0.062     0.188") call abort

write(line, '(RN, 4F10.2)') 0.125_10, 0.375_10, 1.125_10, 1.375_10
if (line.ne."      0.12      0.38      1.12      1.38") call abort

write(line, '(RN, 4F10.1)') 0.25_10, 0.75_10, 1.25_10, 1.75_10
if (line.ne."       0.2       0.8       1.2       1.8") call abort

write(line, '(RN, 4F10.0)') 0.5_10, 1.5_10, 2.5_10, 3.5_10
if (line.ne."        0.        2.        2.        4.") call abort
end
