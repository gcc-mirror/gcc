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
  class(integrand) ,allocatable :: kernel
  type(show_producer) :: executive_producer

  allocate(kernel,source=executive_producer%create_show ())
  if (kernel%variable .ne. 99) call abort
end program
! { dg-final { cleanup-modules "show_producer_class" } }

