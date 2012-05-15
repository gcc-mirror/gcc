! { dg-do run }
! PR51870 - ALLOCATE with class function expression for SOURCE failed.
! This version of the test allocates class arrays.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!
module show_producer_class
  implicit none
  type integrand
    integer :: variable = 0
  end type integrand

  type show_producer
  contains
    procedure ,nopass :: create_show
    procedure ,nopass :: create_show_array
  end type
contains
  function create_show () result(new_integrand)
    class(integrand) ,allocatable :: new_integrand
    allocate(new_integrand)
    new_integrand%variable = -1
  end function
  function create_show_array (n) result(new_integrand)
    class(integrand) ,allocatable :: new_integrand(:)
    integer :: n, i
    allocate(new_integrand(n))
    select type (new_integrand)
      type is (integrand); new_integrand%variable = [(i, i= 1, n)]
    end select
  end function
end module

program main
  use show_producer_class
  implicit none
  class(integrand) ,allocatable :: kernel(:)
  type(show_producer) :: executive_producer

  allocate(kernel(5),source=executive_producer%create_show_array (5))
  select type(kernel)
    type is (integrand);  if (any (kernel%variable .ne. [1,2,3,4,5])) call abort
  end select

  deallocate (kernel)

  allocate(kernel(3),source=executive_producer%create_show ())
  select type(kernel)
    type is (integrand); if (any (kernel%variable .ne. -1)) call abort
  end select
end program
