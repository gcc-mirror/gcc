! { dg-do compile }
! { dg-options "-std=f95" }
! PR 25093: Check that a PUBLIC function can't be of PRIVATE type
! in Fortran 95; in Fortran 2003 it is allowed (cf. PR fortran/38065)
!
module m1

    type :: t1
        integer :: i
    end type t1

    private :: t1
    public :: f1

contains

    type(t1) function f1() ! { dg-error "of PRIVATE derived type" }
    end function

end module

! { dg-final { cleanup-modules "m1" } }
