! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 50564 - this used to ICE with front end optimization.
! Original test case by Andrew Benson.
program test
  implicit none
  double precision, dimension(2) :: timeSteps, control
  integer                        :: iTime
  double precision               :: ratio
  double precision               :: a

  ratio = 0.7d0
  control(1) = ratio**(dble(1)-0.5d0)-ratio**(dble(1)-1.5d0)
  control(2) = ratio**(dble(2)-0.5d0)-ratio**(dble(2)-1.5d0)
  forall(iTime=1:2)
     timeSteps(iTime)=ratio**(dble(iTime)-0.5d0)-ratio**(dble(iTime)-1.5d0)
  end forall
  if (any(abs(timesteps - control) > 1d-10)) STOP 1

  ! Make sure we still do the front-end optimization after a forall
  a = cos(ratio)*cos(ratio) + sin(ratio)*sin(ratio)
  if (abs(a-1.d0) > 1d-10) STOP 2
end program test
! { dg-final { scan-tree-dump-times "__builtin_cos" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_sin" 1 "original" } }
