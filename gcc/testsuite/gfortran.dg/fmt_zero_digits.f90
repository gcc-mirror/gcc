! { dg-do run }
! Verify that when decimal precision is zero, error error given except with 1P.
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! Modified for fix to PR35036
program test
  implicit none
  character(20) :: astr
  integer       :: istat
  50 FORMAT (1PD20.0)
  astr = ""
  write(astr,50) -8.0D0
  if (astr.ne."             -8.D+00") STOP 1
  write(astr,50) 8.0D0
  if (astr.ne."              8.D+00") STOP 2
  write(astr, '(E15.0)', iostat=istat) 1e5
  if (istat /= 5006) STOP 3
  write(astr, '(D15.0)', iostat=istat) 1e5
  if (istat /= 5006) STOP 4
  write(astr, '(G15.0)', iostat=istat) 1e5
  if (istat /= 5006) STOP 5
  write(astr, '(2PE15.0)', iostat=istat) 1e5
  if (istat /= 5006) STOP 6
  write(astr, '(0PE15.0)', iostat=istat) 1e5
  if (istat /= 5006) STOP 7
  write(astr, '(1PE15.0)', iostat=istat) 1e5
  if (istat /= 0) STOP 8
  write(astr, '(F15.0)', iostat=istat) 1e5
  if (astr.ne."        100000.") STOP 9
  if (istat /= 0) STOP 10
end program test
