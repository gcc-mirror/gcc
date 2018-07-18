! { dg-do run }
! PR51870 - ALLOCATE with class function expression for SOURCE failed.
! This version of the test allocates class arrays with MOLD.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!
module show_producer_class
  implicit none
  type integrand
    integer :: variable = 1
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
  class(integrand) ,allocatable :: kernel1(:), kernel2(:)
  type(show_producer) :: executive_producer

  allocate(kernel1(5), kernel2(5),mold=executive_producer%create_show_array (5))
  select type(kernel1)
    type is (integrand);  if (any (kernel1%variable .ne. 1)) STOP 1
  end select

  deallocate (kernel1)

  allocate(kernel1(3),mold=executive_producer%create_show ())
  select type(kernel1)
    type is (integrand); if (any (kernel1%variable .ne. 1)) STOP 2
  end select

  deallocate (kernel1)

  select type(kernel2)
    type is (integrand); kernel2%variable = [1,2,3,4,5]
  end select

  allocate(kernel1(3),source = kernel2(3:5))
  select type(kernel1)
    type is (integrand); if (any (kernel1%variable .ne. [3,4,5])) STOP 3
  end select
end program
