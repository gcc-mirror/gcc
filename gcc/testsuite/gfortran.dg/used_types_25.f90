! { dg-do compile }
!
! Created to check this ambiguity when
! constructors were added. Cf. PR fortran/39427

module m
  type t
  end type t
end module m

use m
 type t ! { dg-error "Derived type definition of 't' at .1. has already been defined" }
 end type t ! { dg-error "Expecting END PROGRAM statement" }
end
