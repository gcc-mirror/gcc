! { dg-additional-options "-Wuninitialized -O0" }
!
! With -O0 only, 'may be uninitalized' warnings show up.
! For the original testcase, compiled with '-O',
! see testcase file 'array-with-dt-1a.f90'

type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)
! { dg-note {'b' declared here} {} { target *-*-* } .-1 }

! Remark: Semantically, the following line requires that 'b'
! is already present on the device.

!$acc update host(b)
! { dg-warning {'b\.dim\[0\]\.ubound' may be used uninitialized} {} { target *-*-* } .-1 }
! { dg-warning {'b\.dim\[0\]\.lbound' may be used uninitialized} {} { target *-*-* } .-2 }

! Remark: Semantically, the following lines require that b is allocated
! and present on the device. The last line also requires the same for 'A'.

!$acc update host(b(:))
!$acc update host(b(1)%A)
!$acc update host(b(1)%A(:,:))
end
