! PR middle-end/107236

! Did ICE before because IFN .GOMP_TARGET_REV was not
! processed in omp-offload.cc.
! Note: Test required ENABLE_OFFLOADING being true inside GCC.

implicit none
!$omp requires reverse_offload
!$omp target parallel num_threads(4)
  !$omp target device(ancestor:1)
    call foo()
  !$omp end target 
!$omp end target parallel
contains
  subroutine foo
  end
end
