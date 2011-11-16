! { dg-do compile }
!
! PR fortran/39427
!
! Check constructor functionality.
!
!
module m
  type t
    integer :: x
  end type t
  interface t
    module procedure f
  end interface t
contains
  function f()
    type(t) :: f
  end function
end module

module m2
  interface t2
    module procedure f2
  end interface t2
  type t2
    integer :: x2
  end type t2
contains
  function f2()
    type(t2) :: f2
  end function
end module

! { dg-final { cleanup-modules "m m2" } }
