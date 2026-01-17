! { dg-do run }
! Test case from the reporter, PR94377
program pdt
  type :: av_t(n)
    integer, len :: n
    integer :: i
    real :: c
    real :: u(n)
  end type av_t
  type(av_t(:)), allocatable :: av(:)
  integer :: k2, k3
  k2 = 3
  k3 = 5
contains
  subroutine al_test(k)
    integer, intent(in) :: k
    integer :: ista
    if (k == 1)  then
      allocate ( av_t(k2) :: av(k3), stat=ista)
      return
    else
      deallocate(av, stat=ista)
    end if
  end subroutine al_test
end program pdt

