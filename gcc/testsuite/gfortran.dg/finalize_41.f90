! { dg-do run }
!
! Test that PR69298 is fixed. Used to segfault on finalization in
! subroutine 'in_type'.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module stuff_mod
  implicit none
  private
  public :: stuff_type, final_calls
  type stuff_type
    private
    integer :: junk
  contains
    procedure get_junk
    procedure stuff_copy_initialiser
    generic :: assignment(=) => stuff_copy_initialiser
    final :: stuff_scalar_finaliser, &
             stuff_1d_finaliser
  end type stuff_type
  integer :: final_calls = 0
  interface stuff_type
    procedure stuff_initialiser
  end interface stuff_type
contains

  function stuff_initialiser( junk ) result(new_stuff)
    implicit none
    type(stuff_type) :: new_stuff
    integer :: junk
    new_stuff%junk = junk
  end function stuff_initialiser

  subroutine stuff_copy_initialiser( destination, source )
    implicit none
    class(stuff_type), intent(out) :: destination
    class(stuff_type), intent(in)  :: source
    destination%junk = source%junk
  end subroutine stuff_copy_initialiser

  subroutine stuff_scalar_finaliser( this )
    implicit none
    type(stuff_type), intent(inout) :: this
    final_calls = final_calls + 1
  end subroutine stuff_scalar_finaliser

  subroutine stuff_1d_finaliser( this )
    implicit none
    type(stuff_type), intent(inout) :: this(:)
    integer :: i
    final_calls = final_calls + 100
  end subroutine stuff_1d_finaliser

  function get_junk( this ) result(junk)
    implicit none
    class(stuff_type), intent(in) :: this
    integer :: junk
    junk = this%junk
  end function get_junk
end module stuff_mod

module test_mod
  use stuff_mod, only : stuff_type, final_calls
  implicit none
  private
  public :: test_type
  type test_type
    private
    type(stuff_type) :: thing
    type(stuff_type) :: things(3)
  contains
    procedure get_value
  end type test_type
  interface test_type
    procedure test_type_initialiser
  end interface test_type
contains

  function test_type_initialiser() result(new_test)
    implicit none
    type(test_type) :: new_test
    integer :: i ! At entry: 1 array and 9 scalars
    new_test%thing = stuff_type( 4 ) ! Gives 2 scalar calls
    do i = 1, 3
      new_test%things(i) = stuff_type( i )  ! Gives 6 scalar calls
    end do
  end function test_type_initialiser

  function get_value( this ) result(value)
    implicit none
    class(test_type) :: this
    integer :: value
    integer :: i
    value = this%thing%get_junk()
    do i = 1, 3
      value = value + this%things(i)%get_junk()
    end do
  end function get_value
end module test_mod

program test
  use stuff_mod, only : stuff_type, final_calls
  use test_mod,  only : test_type
  implicit none
  call here()
! One array call and 1 scalar call after leaving scope => 1 + 9 total; NAGFOR and IFORT agree
  if (final_calls .ne. 109) stop 1
  call in_type()
! 21 calls to scalar finalizer and 4 to the vector version; IFORT agrees
! NAGFOR also produces 21 scalar calls but 5 vector calls.
  if (final_calls .ne. 421) print *, final_calls
contains

  subroutine here()
    implicit none
    type(stuff_type) :: thing
    type(stuff_type) :: bits(3)
    integer :: i
    integer :: tally
    thing = stuff_type(4) ! Two scalar final calls; INTENT(OUT) and initialiser
    do i = 1, 3
      bits(i) = stuff_type(i) ! ditto times 3
    end do
    tally = thing%get_junk()
    do i = 1, 3
      tally = tally + bits(i)%get_junk()
    end do
    if (tally .ne. 10) stop 3 ! 8 scalar final calls by here
  end subroutine here

  subroutine in_type()
    implicit none
    type(test_type) :: thing
    thing = test_type() ! 8 scalar in test_type + 1 vector and 1 scalar to finalize function result and
                        ! 1 vectors and 2 scalars from the expansion of the defined assignment.
    if (thing%get_value() .ne. 10) stop 4
  end subroutine in_type
end program test
