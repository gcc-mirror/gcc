! { dg-do compile }
!
! PR fortran/57435
!
! Contributed by Lorenz Hüdepohl
!
module precision
end module precision
  contains
  use precision     ! { dg-error "Unexpected USE statement in CONTAINS section" }
module stressten_rt ! { dg-error "Unexpected MODULE statement in CONTAINS section" }
  use precision     ! { dg-error "Unexpected USE statement in CONTAINS section" }
  implicit none     ! { dg-error "Unexpected IMPLICIT NONE statement in CONTAINS section" }

! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
