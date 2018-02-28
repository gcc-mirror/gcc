! { dg-do run }
! PR51870 - ALLOCATE with class function expression for SOURCE failed.
! This is the original test in the PR.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!
module show_producer_class
  implicit none
  type integrand
    integer :: variable = -1
  end type integrand

  type show_producer
  contains
    procedure ,nopass :: create_show
  end type
contains
  function create_show () result(new_integrand)
    class(integrand) ,allocatable :: new_integrand
    allocate(new_integrand)
    new_integrand%variable = 99
  end function
end module

program main
  use show_producer_class
  implicit none
  class(integrand) ,allocatable :: kernel1, kernel2
  type(show_producer) :: executive_producer

  allocate(kernel1, kernel2,mold=executive_producer%create_show ())
  if (kernel1%variable .ne. -1) STOP 1
  if (kernel2%variable .ne. -1) STOP 2
end program
