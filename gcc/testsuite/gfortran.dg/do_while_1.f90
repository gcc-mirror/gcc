! { dg-do run }
! PR 50327 - this used to cause an endless loop because
! of wrong fron-end optimization.
program main
  real :: tmp
  tmp = 0.
  do while (abs(tmp) < 10. .and. abs(tmp) < 20.)
     tmp = tmp + 1.
  end do
end program main
