! { dg-do compile }
! PR 34549 - a real value was accepted for shift.
program main
  implicit none
  real, dimension(2,2) :: r
  data r /1.0, 2.0, 3.0, 4.0/
  print *,cshift(r,shift=2.3,dim=1) ! { dg-error "must be INTEGER" }
end program main
