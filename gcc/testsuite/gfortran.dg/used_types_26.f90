! { dg-do compile }
!
! Check for ambiguity.
!
! Added as part of the constructor work (PR fortran/39427).
!
  module m
    type t
    end type t
  end module m

  module m2
    type t
    end type t
  end module m2

  use m
  use m2
  type(t) :: x ! { dg-error "Type name 't' at .1. is ambiguous" }
  end

! { dg-final { cleanup-modules "m m2" } }
