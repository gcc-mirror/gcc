! PR middle-end/89621
! { dg-do compile }

subroutine sub(str)
  character(*), intent(in) :: str
end subroutine sub

program pr89621
  implicit none
  integer i
  character(len=:), allocatable :: str
  str = "test"
  !$omp parallel do
  do i = 1, 10
    call sub(str)
  enddo
  !$omp end parallel do
end program pr89621
