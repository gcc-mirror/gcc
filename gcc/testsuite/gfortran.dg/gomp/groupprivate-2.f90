! { dg-additional-options "-Wno-deprecated-openmp" }

module m
 implicit none
 integer :: ii
 integer :: x, y(20), z, v, q, r,o, b2,c

 !$omp groupprivate(x, z, o) device_Type( any )
 !$omp declare target enter(x) device_type ( any )  ! { dg-error "List item 'x' at .1. not appear in the 'enter' clause as it was previously specified in a GROUPPRIVATE directive" }
 !$omp declare target to(z) device_type ( any )  ! { dg-error "List item 'z' at .1. not appear in the 'to' clause as it was previously specified in a GROUPPRIVATE directive" }
 !$omp declare target link(o) device_type ( any )  ! { dg-error "List item 'o' at .1. not appear in the 'link' clause as it was previously specified in a GROUPPRIVATE directive" }
 !$omp declare target enter( ii) ,local(y,c), link(r), to(q) device_type ( host )
 !$omp groupprivate(r,q) device_type(host)
! { dg-error "List item 'q' at .1. implies OMP DECLARE TARGET with the LOCAL clause, but it has been specified with a different clause before" "" { target *-*-* } .-1 }
! { dg-error "List item 'r' at .1. implies OMP DECLARE TARGET with the LOCAL clause, but it has been specified with a different clause before" "" { target *-*-* } .-2 }
 !$omp groupprivate(c) ! { dg-error "List item 'c' at .1. set in previous OMP DECLARE TARGET directive to the different DEVICE_TYPE 'host'" }
 !$omp groupprivate(y) device_type( any)  ! { dg-error "List item 'y' at .1. set in previous OMP DECLARE TARGET directive to the different DEVICE_TYPE 'host'" }
 !$omp groupprivate(v) device_type (nohost )
 !$omp groupprivate(v)  ! { dg-error "Duplicate OpenMP GROUPPRIVATE attribute specified" }

 !$omp declare target link(b2) device_type(nohost)  ! { dg-error "List item 'b2' at .1. set with NOHOST specified may not appear in a LINK clause" }
end module

subroutine sub()
  implicit none
  integer, save :: x0,x1,x2,x3,x4
  !$omp groupprivate(x0)
  !$omp groupprivate(x1)
  !$omp groupprivate(x2) device_type ( any)
  !$omp groupprivate(x3) device_type (host )
  !$omp groupprivate(x4) device_type( nohost)

  !$omp declare target(x0)  ! { dg-error "List item 'x0' at .1. not appear in the 'enter' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(any) to(x1)  ! { dg-error "List item 'x1' at .1. not appear in the 'to' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(any) enter(x2)  ! { dg-error "List item 'x2' at .1. not appear in the 'enter' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(host) link(x3)  ! { dg-error "List item 'x3' at .1. not appear in the 'link' clause as it was previously specified in a GROUPPRIVATE directive" }
  !$omp declare target device_type(host) local(x4) ! { dg-error "List item 'x4' at .1. set in previous OMP GROUPPRIVATE directive to the different DEVICE_TYPE 'nohost'" }

end
