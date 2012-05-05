! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/53111
!

! ------------ INVALID ONE ------------------------

module m
type t
  integer :: i
end type t
end

module m2
 interface t
   module procedure sub
 end interface t
contains
 integer function sub()
   sub = 4
 end function sub
end module m2

! Note: The following is formally valid as long as "t" is not used.
! For simplicity, -std=f95 will give an error.
! It is unlikely that a real-world program is rejected with -std=f95
! because of that.

use m   ! { dg-error "Fortran 2003: Generic name 't' of function 'sub' at .1. being the same name as derived type at" }
use m2  ! { dg-error "Fortran 2003: Generic name 't' of function 'sub' at .1. being the same name as derived type at" }
! i = sub()  ! << Truly invalid in F95, valid in F2003
end

! ------------ INVALID TWO ------------------------

module m3
type t2  ! { dg-error "Fortran 2003: Generic name 't2' of function 'sub2' at .1. being the same name as derived type at" }
  integer :: i
end type t2
 interface t2
   module procedure sub2
 end interface t2
contains
 integer function sub2()  ! { dg-error "Fortran 2003: Generic name 't2' of function 'sub2' at .1. being the same name as derived type at" }
   sub2 = 4
 end function sub2
end module m3
