! { dg-do run }
! PR28354 Incorrect rounding of .99999 with f3.0 format specifier
! Test case derived from PR. Submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>
  write(*,50) -0.99999
  write(*,50) 0.99999
  write(*,50) -9.0
  write(*,50) -0.99
  write(*,50) -0.999
  write(*,50) -0.999
  write(*,50) -0.59
  write(*,50) -0.49
  write(*,100) 37.99999
  write(*,100) 10345.0
  write(*,100) 333.678
  write(*,100) 333.499
  50   format(f3.0,"<")
 100   format(f8.0,"<")
  end
! {dg-output "-1.<"
! {dg-output " 1.<"
! {dg-output "-9.<"
! {dg-output "-1.<"
! {dg-output "-1.<"
! {dg-output "-1.<"
! {dg-output "-1.<"
! {dg-output " 0.<"
! {dg-output "     38.<"
! {dg-output "  10345.<"
! {dg-output "    334.<"
! {dg-output "    333.<"
