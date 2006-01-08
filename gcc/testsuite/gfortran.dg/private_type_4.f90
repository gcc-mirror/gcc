! { dg-do compile }
! PR 25093: Check that a PUBLIC function can't be of PRIVATE type
module m1

    type :: t1
        integer :: i
    end type t1

    private :: t1
    public :: f1     ! { dg-error "cannot be of PRIVATE type" }

contains

    type(t1) function f1()
    end function

end module
