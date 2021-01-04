! { dg-do run }
!
! Test the fix for PR96320 in which the assumed shape of 'arg' in the
! interface for 'bar' was mirrored by the 'arg' in the module procedure
! incorrectly have deferred shape.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module foobar
  type foo
  contains
    procedure, nopass :: bar1
    procedure, nopass :: bar2
    procedure, nopass :: bar3
  end type

  interface

    module subroutine bar1(arg)
      character(len=*) arg(:)
    end subroutine

    module subroutine bar2(arg)
      character(len=*) arg(3:)
    end subroutine

    module subroutine bar3(arg)
      character(len=*) arg(2)
    end subroutine

  end interface
contains

  module procedure bar1
    if (lbound(arg, 1) .ne. 1) stop 1
    if (arg(3) .ne. 'hijk') stop 2
  end procedure

! Make sure that the lower bound of an assumed shape array dummy,
! if defined, is passed to the module procedure.

  module procedure bar2
    if (lbound(arg, 1) .ne. 3) stop 3
    if (arg(3) .ne. 'abcd') stop 4
  end procedure

! This makes sure that an dummy with explicit shape has the upper
! bound correctly set in the module procedure.

  module procedure bar3
    if (lbound(arg, 1) .ne. 1) stop 5
    if (arg(3) .ne. 'hijk') stop 6       ! { dg-warning "is out of bounds" }
  end procedure

end module

  use foobar
  character(4) :: list(3) = ['abcd', 'efgh' , 'hijk']
  type(foo) :: f
  call f%bar1(list)
  call f%bar2(list)
  call f%bar3(list)
end
