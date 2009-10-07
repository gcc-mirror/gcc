! { dg-do run }
! PR35962 Implement F2003 rounding modes.
! Test case prepared by Jerry Delisle  <jvdelisle@gcc.gnu.org>
integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
character(64) :: line
write(line, '(RN, 4F10.3)') 0.0625_k, 0.1875_k
if (line.ne."     0.062     0.188") call abort

write(line, '(RN, 4F10.2)') 0.125_k, 0.375_k, 1.125_k, 1.375_k
if (line.ne."      0.12      0.38      1.12      1.38") call abort

write(line, '(RN, 4F10.1)') 0.25_k, 0.75_k, 1.25_k, 1.75_k
if (line.ne."       0.2       0.8       1.2       1.8") call abort

write(line, '(RN, 4F10.0)') 0.5_k, 1.5_k, 2.5_k, 3.5_k
if (line.ne."        0.        2.        2.        4.") call abort
end
