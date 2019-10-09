! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-optimized -O0" }

! Check use_device_ptr with local variables

module offloading
  use iso_c_binding
  implicit none
  interface
    subroutine copy3_array_data(from, to, N) bind(C)
      import :: c_ptr
      type(c_ptr), value :: from, to
      integer, value :: N
    end subroutine copy3_array_data
  end interface
end module offloading

subroutine omp_device_ptr()
  use iso_c_binding
  use offloading
  implicit none

  integer, parameter :: N = 1000
  real(c_double), pointer :: AA(:), BBB(:)
  real(c_double), allocatable, target :: CC(:), DD(:)
  real(c_double), target :: EE(N), FF(N), dummy(1)

  ! allocate(AA(N), BBB(N), CC(N), DD(N))  ! make dump more readable

  ! AA = 11.0_c_double
  ! BBB = 22.0_c_double
  ! CC = 33.0_c_double
  ! DD = 44.0_c_double
  ! EE = 55.0_c_double
  ! FF = 66.0_c_double

  ! NOTE: OpenMP 5's use_device_addr is (at time of writing) not yet supported

  ! pointer-type array to use_device_ptr
  ! !$omp target data map(to:AA) map(from:BBB) use_device_ptr(AA,BBB)
  !$omp target data map(alloc:dummy) use_device_ptr(AA,BBB)
  call copy3_array_data(c_loc(AA), c_loc(BBB), N)
  !$omp end target data

  ! allocatable array to use_device_ptr
  !!$omp target data map(to:CC) map(from:DD) use_device_ptr(CC,DD)
  !$omp target data map(alloc:dummy) use_device_ptr(CC,DD)
  call copy3_array_data(c_loc(CC), c_loc(DD), N)
  !$omp end target data

  ! fixed-size decriptorless array to use_device_ptr
  !!$omp target data map(to:EE) map(from:FF) use_device_ptr(EE,FF)
  !$omp target data map(alloc:dummy) use_device_ptr(EE,FF)
  call copy3_array_data(c_loc(EE), c_loc(FF), N)
  !$omp end target data

  ! deallocate(AA, BBB)  ! Free all pointers, only
end subroutine omp_device_ptr


! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.aa = &aa" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.aa = aa" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.bbb = &bbb" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.bbb = bbb" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.cc = &cc" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.cc = cc" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.dd = &dd" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.dd = dd" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.ee = ee" "optimized" } }
! { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9\]+.ff = ff" "optimized" } }
! { dg-final { scan-tree-dump-times ".omp_data_arr.\[0-9\]+.ee = &ee" 1 "optimized" } }
! { dg-final { scan-tree-dump-times ".omp_data_arr.\[0-9\]+.ff = &ff" 1 "optimized" } }

! { dg-final { scan-tree-dump-times "_\[0-9\]+ = aa.data;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "aa\.\[0-9\]+ = aa;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "aa\.\[0-9\]+\.data = _\[0-9\]+;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = aa\.\[0-9\]+\.data;" 1 "optimized" } }

! { dg-final { scan-tree-dump-times "_\[0-9\]+ = bbb.data;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "bbb\.\[0-9\]+ = bbb;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "bbb\.\[0-9\]+\.data = _\[0-9\]+;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = bbb\.\[0-9\]+\.data;" 1 "optimized" } }

! '3' because of automatic deallocation
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = cc.data;" 3 "optimized" } }
! { dg-final { scan-tree-dump-times "cc\.\[0-9\]+ = cc;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "cc\.\[0-9\]+\.data = _\[0-9\]+;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = cc\.\[0-9\]+\.data;" 1 "optimized" } }

! '3' because of automatic deallocation
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = dd.data;" 3 "optimized" } }
! { dg-final { scan-tree-dump-times "dd\.\[0-9\]+ = dd;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "dd\.\[0-9\]+\.data = _\[0-9\]+;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = dd\.\[0-9\]+\.data;" 1 "optimized" } }

! { dg-final { scan-tree-dump-times "_\[0-9\]+ = .omp_data_arr.\[0-9\]+.ee;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "ee\.\[0-9\]+_\[0-9\]+ = _\[0-9\]+;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = &\\*ee.\[0-9\]+_\[0-9\]+;" 1 "optimized" } }

! { dg-final { scan-tree-dump-times "_\[0-9\]+ = .omp_data_arr.\[0-9\]+.ff;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "ff\.\[0-9\]+_\[0-9\]+ = _\[0-9\]+;" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "_\[0-9\]+ = &\\*ff.\[0-9\]+_\[0-9\]+;" 1 "optimized" } }
