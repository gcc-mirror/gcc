! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple -fdump-tree-ompdevlow" }

subroutine f (a, num)
  integer, parameter :: N = 256
  integer :: a(N)
  integer :: num
  integer :: i

  !$omp metadirective &
  !$omp& when (target_device={device_num(num), kind("gpu"), arch("nvptx")}: &
  !$omp&       target parallel do map(tofrom: a(1:N))) &
  !$omp& when (target_device={device_num(num), kind("gpu"), &
  !$omp&                      arch("amdgcn"), isa("gfx906")}: &
  !$omp&       target parallel do) &
  !$omp& when (target_device={device_num(num), kind("cpu"), arch("x86_64")}: &
  !$omp&       parallel do)
    do i = 1, N
      a(i) = a(i) + i
    end do

  !$omp metadirective &
  !$omp& when (target_device={kind("gpu"), arch("nvptx")}: &
  !$omp&       target parallel do map(tofrom: a(1:N)))
    do i = 1, N
      a(i) = a(i) + i
    end do
end subroutine

! For configurations with offloading, we expect one "pragma omp target"
! with "device(num)" for each target_device selector that specifies
! "device_num(num)".  Without offloading, there should be zero as the
!  resolution happens during gimplification.
! { dg-final { scan-tree-dump-times "pragma omp target\[^\\n\]* device\\(" 3 "gimple" { target offloading_enabled } } }
! { dg-final { scan-tree-dump-times "pragma omp target\[^\\n\]* device\\(" 0 "gimple" { target { ! offloading_enabled } } } }

! For configurations with offloading, expect one OMP_TARGET_DEVICE_MATCHES
! for each kind/arch/isa selector.  These are supposed to go away after
!  ompdevlow.
! { dg-final { scan-tree-dump-times "OMP_TARGET_DEVICE_MATCHES" 9 "gimple" { target offloading_enabled } } }
! { dg-final { scan-tree-dump-times "OMP_TARGET_DEVICE_MATCHES" 0 "gimple" { target { ! offloading_enabled } } } }
! { dg-final { scan-tree-dump-times "OMP_TARGET_DEVICE_MATCHES" 0 "ompdevlow" { target offloading_enabled } } }
