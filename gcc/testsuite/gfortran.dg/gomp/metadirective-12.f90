! { dg-do compile }

! PR112779 item H; this testcase used to ICE.

program test
  implicit none
  integer, parameter :: N = 100
  integer :: x(N), y(N), z(N)
  block
    integer :: i
    !$omp metadirective &
                !$omp& when(device={arch("nvptx")}: teams loop) &
                !$omp& default(parallel loop)
    do i = 1, N
          z(i) = x(i) * y(i)
    enddo
   end block
end
