! { dg-do run }
!
! Test the fix for PR84546 in which the failing cases would
! have x%vec = ['foo','b   '].
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module any_vector_type

  type :: any_vector
    class(*), allocatable :: vec(:)
  end type

  interface any_vector
    procedure any_vector1
  end interface

contains

  function any_vector1(vec) result(this)
    class(*), intent(in) :: vec(:)
    type(any_vector) :: this
    allocate(this%vec, source=vec)
  end function

end module

program main

  use any_vector_type
  implicit none

  class(*), allocatable :: x
  character(*), parameter :: vec(2) = ['foo','bar']
  integer :: vec1(3) = [7,8,9]

  call foo1
  call foo2
  call foo3
  call foo4

contains

  subroutine foo1 ! This always worked
    allocate (any_vector :: x)
    select type (x)
      type is (any_vector)
        x = any_vector(vec)
    end select
    call bar(1)
    deallocate (x)
  end

  subroutine foo2 ! Failure found during diagnosis
    x = any_vector (vec)
    call bar(2)
    deallocate (x)
  end

  subroutine foo3 ! Original failure
    allocate (x, source = any_vector (vec))
    call bar(3)
    deallocate (x)
  end

  subroutine foo4 ! This always worked
    allocate (x, source = any_vector (vec1))
    call bar(4)
    deallocate (x)
  end

  subroutine bar (stop_flag)
    integer :: stop_flag
    select type (x)
      type is (any_vector)
        select type (xvec => x%vec)
          type is (character(*))
            if (any(xvec /= vec)) stop stop_flag
          type is (integer)
            if (any(xvec /= (vec1))) stop stop_flag
        end select
    end select
  end
end program
