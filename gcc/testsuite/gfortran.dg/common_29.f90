! { dg-do compile }
! PR fortran/108454
!
! Contributed by G.Steinmetz

module m
  type t
  end type
contains
  subroutine s
    common t
  end
end

module m2
  implicit none
  type t
  end type
contains
  subroutine s
    real :: t
    common /com/ t
  end
end

module m3
  type t
  end type
contains
  subroutine s
    type(t) :: x  ! { dg-error "cannot be host associated at .1." }
    common t      ! { dg-error "incompatible object of the same name" }
  end
end
