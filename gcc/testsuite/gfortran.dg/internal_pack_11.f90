! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! Test the fix for PR43173, where unnecessary calls to internal_pack/unpack
! were being produced below. These references are contiguous and so do not
! need a temporary.
!
! Contributed Tobias Burnus <burnus@gcc.gnu.org>
!
  REAL, allocatable :: ot(:)
  integer :: time_steps

  call foo (ot) ! OK, no temporary
  call foo (ot(0:5:1)) ! Was an unnecessary temporary
  call foo (ot(0:time_steps)) ! Was an unnecessary temporary
  end
! { dg-final { scan-tree-dump-times "unpack" 0 "original" } }
