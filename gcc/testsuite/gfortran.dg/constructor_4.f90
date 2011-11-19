! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/39427
!
! Check constructor functionality.
!
!
module m
  type t ! { dg-error "the same name as derived type" }
    integer :: x
  end type t
  interface t
    module procedure f
  end interface t
contains
  function f() ! { dg-error "the same name as derived type" }
    type(t) :: f
  end function
end module

module m2
  interface t2
    module procedure f2
  end interface t2
  type t2 ! { dg-error "the same name as derived type" }
    integer :: x2
  end type t2
contains
  function f2() ! { dg-error "the same name as derived type" }
    type(t2) :: f2
  end function
end module
