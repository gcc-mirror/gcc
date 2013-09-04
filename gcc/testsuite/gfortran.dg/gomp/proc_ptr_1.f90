! { dg-do compile }
!
! PR 46271: [F03] OpenMP default(none) and procedure pointers
!
! Contributed by Marco Restelli <mrestelli@gmail.com>

program test
  implicit none
  integer :: i
  real :: s(1000)
  procedure(f), pointer :: pf
 
  pf => f

  !$omp parallel do schedule(static) private(i) shared(s,pf) default(none)
  do i=1,1000
    call pf(real(i),s(i))
  enddo
  !$omp end parallel do

  write(*,*) 'Sum ',sum(s)
contains
  pure subroutine f(x,y)
    real, intent(in) :: x
    real, intent(out) :: y
    y = sin(x)*cos(x)
  end subroutine
end
