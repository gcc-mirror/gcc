! { dg-do compile }

implicit none

type t
integer :: foo
character(len=8) :: bar
integer :: qux(5)
end type t

type(t) :: var

var%foo = 3
var%bar = "HELLOOMP"
var%qux = (/ 1, 2, 3, 4, 5 /) 

!$acc enter data copyin(var)

!$acc enter data attach(var%foo)
! { dg-error "'attach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }
!$acc enter data attach(var%bar)
! { dg-error "'attach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }
!$acc enter data attach(var%qux)
! { dg-error "'attach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }

!$acc serial
var%foo = 5
var%bar = "GOODBYE!"
var%qux = (/ 6, 7, 8, 9, 10 /)
!$acc end serial

!$acc exit data detach(var%qux)
! { dg-error "'detach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }
!$acc exit data detach(var%bar)
! { dg-error "'detach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }
!$acc exit data detach(var%foo)
! { dg-error "'detach' clause argument must be ALLOCATABLE or a POINTER" "" { target *-*-* } .-1 }

!$acc exit data copyout(var)

if (var%foo.ne.5) stop 1
if (var%bar.ne."GOODBYE!") stop 2

end
