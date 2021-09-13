! { dg-do run }
!
! Test the fix for PR98342.
!
! Contributed by Martin Stein  <mscfd@gmx.net>
!
module mod
  implicit none
  private
  public get_tuple, sel_rank1, sel_rank2, sel_rank3

  type, public :: tuple
  integer, dimension(:), allocatable :: t
end type tuple

contains

function sel_rank1(x) result(s)
  character(len=:), allocatable :: s
  type(tuple), dimension(..), intent(in) :: x
  select rank (x)
    rank (0)
      s = '10'
    rank (1)
      s = '11'
    rank default
      s = '?'
  end select
end function sel_rank1

function sel_rank2(x) result(s)
  character(len=:), allocatable :: s
  class(tuple), dimension(..), intent(in) :: x
  select rank (x)
    rank (0)
      s = '20'
    rank (1)
      s = '21'
    rank default
      s = '?'
  end select
end function sel_rank2

function sel_rank3(x) result(s)
  character(len=:), allocatable :: s
  class(*), dimension(..), intent(in) :: x
  select rank (x)
    rank (0)
      s = '30'
    rank (1)
      s = '31'
    rank default
      s = '?'
  end select
end function sel_rank3

function get_tuple(t) result(a)
  type(tuple) :: a
  integer, dimension(:), intent(in) :: t
  allocate(a%t, source=t)
end function get_tuple

end module mod


program alloc_rank
  use mod
  implicit none

  integer, dimension(1:3) :: x
  character(len=:), allocatable :: output
  type(tuple) :: z

  x = [1,2,3]
  z = get_tuple (x)
                                       ! Derived type formal arg
  output = sel_rank1(get_tuple (x))    ! runtime: Error in `./alloc_rank.x':
  if (output .ne. '10') stop 1
  output = sel_rank1([z])              ! This worked OK
  if (output .ne. '11') stop 2

                                       ! Class formal arg
  output = sel_rank2(get_tuple (x))    ! runtime: Error in `./alloc_rank.x':
  if (output .ne. '20') stop 3
  output = sel_rank2([z])              ! This worked OK
  if (output .ne. '21') stop 4

                                       ! Unlimited polymorphic formal arg
  output = sel_rank3(get_tuple (x))    ! runtime: Error in `./alloc_rank.x':
  if (output .ne. '30') stop 5
  output = sel_rank3([z])              ! runtime: segmentation fault
  if (output .ne. '31') stop 6

  deallocate (output)
  deallocate (z%t)
end program alloc_rank
