! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }
!
! PR middle-end/128001
!
! Based on OpenMP example-document's metadirective.2.f90
!

! Ensure 'work_on_chunk' is marked as 'omp declare target'
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare target\\)\\)\[\\r\\n\]+__attribute__\\(\\(fn spec \\(\"\\. w w \"\\)\\)\\)\[\\r\\n\]+void work_on_chunk" "gimple" } }

program main
    !use omp_lib
    implicit none
    integer, parameter :: N = 1000
    external :: work_on_chunk
    integer :: i, idev

    do idev = 0, 0 ! omp_get_num_devices() - 1

        !$omp target device(idev)
        !$omp begin metadirective &
        !$omp&  when( implementation={vendor(nvidia)},        &
        !$omp&           device={arch("kepler")}:             &
        !$omp&        teams num_teams(512) thread_limit(32) ) &
        !$omp&  when( implementation={vendor(amd)},           &
        !$omp&           device={arch("fiji"  )}:             &
        !$omp&        teams num_teams(512) thread_limit(64) ) &
        !$omp&  otherwise( teams )
        !$omp distribute parallel do
        do i = 1, N
            call work_on_chunk(idev, i)
        end do
        !$omp end metadirective
        !$omp end target

    end do

end program

subroutine work_on_chunk(idev, i)
    integer :: i, idev
end subroutine
