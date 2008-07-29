! { dg-do compile }
! Some errors for derived type extension.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module m
  use iso_c_binding
  type :: date
    sequence
    integer :: yr, mon
    integer,public :: day
  end type
  type, bind(c) :: dt
    integer(c_int) :: yr, mon
    integer(c_int) :: day
  end type
end module m

  use m
  type, extends(date) :: datetime ! { dg-error "because it is a SEQUENCE type" }
  end type ! { dg-error "Expecting END PROGRAM" }

  type, extends(dt) :: dt_type ! { dg-error "because it is BIND" }
  end type ! { dg-error "Expecting END PROGRAM" }
end

! { dg-final { cleanup-modules "m" } }
