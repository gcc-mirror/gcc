! { dg-do run }
!
! PR fortran/39427
!
! Contributed by Norman S. Clerman (in PR fortran/45155)
!
! Constructor test case
!
!
module test_cnt
  integer, public, save :: my_test_cnt = 0
end module test_cnt

module Rational
  use test_cnt
  implicit none
  private

  type, public :: rational_t
    integer :: n = 0, id = 1
  contains
    procedure, nopass :: Construct_rational_t
    procedure :: Print_rational_t
    procedure, private :: Rational_t_init
    generic :: Rational_t => Construct_rational_t
    generic :: print      => Print_rational_t
  end type rational_t

contains

  function Construct_rational_t (message_) result (return_type)
    character (*), intent (in) :: message_
    type (rational_t) :: return_type

!    print *, trim (message_)
    if (my_test_cnt /= 1) call abort()
    my_test_cnt = my_test_cnt + 1
    call return_type % Rational_t_init

  end function Construct_rational_t

  subroutine Print_rational_t (this_)
    class (rational_t), intent (in) :: this_

!    print *, "n, id", this_% n, this_% id
    if (my_test_cnt == 0) then
      if (this_% n /= 0 .or. this_% id /= 1) call abort ()
    else if (my_test_cnt == 2) then
      if (this_% n /= 10 .or. this_% id /= 0) call abort ()
    else
      call abort ()
    end if
    my_test_cnt = my_test_cnt + 1
  end subroutine Print_rational_t

  subroutine Rational_t_init (this_)
    class (rational_t), intent (in out) :: this_

    this_% n = 10
    this_% id = 0

  end subroutine Rational_t_init

end module Rational

module Temp_node
  use test_cnt
  implicit none
  private

  real, parameter :: NOMINAL_TEMP = 20.0

  type, public :: temp_node_t
    real :: temperature = NOMINAL_TEMP
    integer :: id = 1
  contains
    procedure :: Print_temp_node_t
    procedure, private :: Temp_node_t_init
    generic :: Print => Print_temp_node_t
  end type temp_node_t

  interface temp_node_t
    module procedure Construct_temp_node_t
  end interface

contains

  function Construct_temp_node_t (message_) result (return_type)
    character (*), intent (in) :: message_
    type (temp_node_t) :: return_type

    !print *, trim (message_)
    if (my_test_cnt /= 4) call abort()
    my_test_cnt = my_test_cnt + 1
    call return_type % Temp_node_t_init

  end function Construct_temp_node_t

  subroutine Print_temp_node_t (this_)
    class (temp_node_t), intent (in) :: this_

!    print *, "temp, id", this_% temperature, this_% id
    if (my_test_cnt == 3) then
      if (this_% temperature /= 20 .or. this_% id /= 1) call abort ()
    else if (my_test_cnt == 5) then
      if (this_% temperature /= 10 .or. this_% id /= 0) call abort ()
    else
      call abort ()
    end if
    my_test_cnt = my_test_cnt + 1
  end subroutine Print_temp_node_t

  subroutine Temp_node_t_init (this_)
    class (temp_node_t), intent (in out) :: this_

    this_% temperature = 10.0
    this_% id = 0

  end subroutine Temp_node_t_init

end module Temp_node

program Struct_over
  use test_cnt
  use Rational,  only : rational_t
  use Temp_node, only : temp_node_t

  implicit none

  type (rational_t)  :: sample_rational_t
  type (temp_node_t) :: sample_temp_node_t

!  print *, "rational_t"
!  print *, "----------"
!  print *, ""
!
!  print *, "after declaration"
  if (my_test_cnt /= 0) call abort()
  call sample_rational_t % print

  if (my_test_cnt /= 1) call abort()

  sample_rational_t = sample_rational_t % rational_t ("using override")
  if (my_test_cnt /= 2) call abort()
!  print *, "after override"
  !  call print (sample_rational_t)
  !  call sample_rational_t % print ()
  call sample_rational_t % print

  if (my_test_cnt /= 3) call abort()

!  print *, "sample_t"
!  print *, "--------"
!  print *, ""
!
!  print *, "after declaration"
  call sample_temp_node_t % print

  if (my_test_cnt /= 4) call abort()

  sample_temp_node_t = temp_node_t ("using override")
  if (my_test_cnt /= 5) call abort()
!  print *, "after override"
  !  call print (sample_rational_t)
  !  call sample_rational_t % print ()
  call sample_temp_node_t % print
  if (my_test_cnt /= 6) call abort()

end program Struct_over

! { dg-final { cleanup-modules "test_cnt rational temp_node" } }
