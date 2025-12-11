! { dg-additional-options "-Wno-deprecated-openmp" }

module m
 implicit none
 integer :: ii
 integer :: x, y(20), z, v, q, r,o, b2,c

 common /b_ii/ ii
 common /b_x/ x
 common /b_y/ y
 common /b_z/ z
 common /b_v/ v
 common /b_q/ q
 common /b_r/ r
 common /b_o/ o
 common /b_b2/ b2
 common /b_c/ c

 !$omp groupprivate(/b_x/, /b_z/, /b_o/) device_Type( any )
 !$omp declare target enter(/b_x/) device_type ( any )  ! { dg-error "Common block '/b_x/' at .1. not appear in the 'enter' clause as it was previously specified in a GROUPPRIVATE directive" }
 !$omp declare target to(/b_z/) device_type ( any )  ! { dg-error "Common block '/b_z/' at .1. not appear in the 'to' clause as it was previously specified in a GROUPPRIVATE directive" }
 !$omp declare target link(/b_o/) device_type ( any )  ! { dg-error "Common block '/b_o/' at .1. not appear in the 'link' clause as it was previously specified in a GROUPPRIVATE directive" }
 !$omp declare target enter( / b_ii / ) ,local(/b_y/ , /b_c/), link(/b_r/), to(/b_q/) device_type ( host )
 !$omp groupprivate( /b_r/ ,/b_q/) device_type(host)
! { dg-error "List item '/b_r/' at .1. implies OMP DECLARE TARGET with the LOCAL clause, but it has been specified with a different clause before" "" { target *-*-* } .-1 }
! { dg-error "List item '/b_q/' at .1. implies OMP DECLARE TARGET with the LOCAL clause, but it has been specified with a different clause before" "" { target *-*-* } .-2 }
 !$omp groupprivate(/b_c/)  ! { dg-error "List item 'b_c' at .1. set in previous OMP DECLARE TARGET directive to the different DEVICE_TYPE 'host'" }
 !$omp groupprivate(/b_y/) device_type( any)  ! { dg-error "List item 'b_y' at .1. set in previous OMP DECLARE TARGET directive to the different DEVICE_TYPE 'host'" }
 !$omp groupprivate(/b_v/) device_type (nohost )
 !$omp groupprivate(/b_v/)  ! { dg-error "Duplicate OpenMP GROUPPRIVATE attribute specified at .1." }

 !$omp declare target link(/b_b2/) device_type(nohost)  ! { dg-error "Common block '/b_b2/' at .1. set with NOHOST specified may not appear in a LINK clause" }
end module

subroutine sub()
  implicit none
  integer, save :: xx
  integer :: x0,x1,x2,x3,x4

  common /b_xx/ xx  ! { dg-error "COMMON attribute conflicts with SAVE attribute in 'xx' at .1." }
  common /b_x0/ x0
  common /b_x1/ x1
  common /b_x2/ x2
  common /b_x3/ x3
  common /b_x4/ x4

  !$omp groupprivate(/b_xx/)  ! { dg-error "COMMON attribute conflicts with SAVE attribute in 'xx' at .1." }
  !$omp groupprivate(/b_x0/)
  !$omp groupprivate(/b_x1/)
  !$omp groupprivate(/b_x2/) device_type ( any)
  !$omp groupprivate(/b_x3/) device_type (host )
  !$omp groupprivate(/b_x4/) device_type( nohost)

  !$omp declare target(/b_x0/)  ! { dg-error "Common block '/b_x0/' at .1. not appear in the 'enter' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(any) to(/b_x1/)  ! { dg-error "Common block '/b_x1/' at .1. not appear in the 'to' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(any) enter(/b_x2/)  ! { dg-error "Common block '/b_x2/' at .1. not appear in the 'enter' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(host) link(/b_x3/)  ! { dg-error "Common block '/b_x3/' at .1. not appear in the 'link' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(host) local(/b_x4/) ! { dg-error "Common block '/b_x4/' at .1. set in previous OMP GROUPPRIVATE directive to the different DEVICE_TYPE 'nohost'" }

end
