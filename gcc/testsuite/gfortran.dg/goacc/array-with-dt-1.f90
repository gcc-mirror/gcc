! { dg-additional-options -Wuninitialized }

! Purpose of this testcase (from the commit log):
!   This patch fixes lowering of derived-type mappings which select elements
!   of arrays of derived types, and similar. These would previously lead
!   to ICEs.

! This testcase does not show any uninitialized warnings when compiled with -O
! (as done). For -O0, see testcase file 'array-with-dt-1a.f90'.

type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)

! Remark: Semantically, the following line requires that 'b'
! is already present on the device.

!$acc update host(b)

! Remark: Semantically, the following lines require that b is allocated
! and present on the device. The last line also requires the same for 'A'.

!$acc update host(b(:))
!$acc update host(b(1)%A)
!$acc update host(b(1)%A(:,:))
end
