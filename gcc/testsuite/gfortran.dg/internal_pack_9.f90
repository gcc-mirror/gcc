! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! During the discussion of the fix for PR43072, in which unnecessary
! calls to internal PACK/UNPACK were being generated, the following,
! further unnecessary temporaries or PACk/UNPACK were found.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
! Case 1: Substring encompassing the whole string
subroutine foo2
  implicit none
  external foo
  character(len=20) :: str(2) = '1234567890'
  call foo(str(:)(1:20)) ! This is still not fixed.
end

! Case 2: Contiguous array section
subroutine bar
  implicit none
  external foo
  integer :: a(3,3,3)
  call foo(a(:,:,:)) ! OK, no temporary
  call foo(a(:,:,1)) ! OK, no temporary
  call foo(a(:,2,2)) ! Used unnecessarily a temporary -FIXED
  call foo(a(2,:,1)) ! OK, creates a temporary(1)
end

! Case 3: Stride 1 section.
subroutine foobar
  implicit none
  external foo
  integer :: A(10,10)
  call foo(A(3:7,4)) ! Used unnecessarily a temporary - FIXED
  call foo(A(:,3:7)) ! OK (no temporary)
  call foo(A(1:10,3:7)) ! OK (no temporary)
  call foo(A(4,3:7)) ! temporary OK(2)
  call foo(A(:,3:7:-1)) ! temporary(3) OK because of stride
end
! { dg-final { scan-tree-dump-times "unpack" 3 "original" } }
