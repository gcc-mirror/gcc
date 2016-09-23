! { dg-do compile }
! { dg-options "-fdec-static" }
!
! Check for conflicts between STATIC/AUTOMATIC and other attributes.
!

function s(a, b, x, y) result(z)
  implicit none
  integer, automatic, intent(IN) :: a ! { dg-error "DUMMY attribute conflicts" }
  integer, static, intent(IN) :: b ! { dg-error "DUMMY attribute conflicts" }
  integer, intent(OUT) :: x, y
  automatic :: x ! { dg-error "DUMMY attribute conflicts" }
  static :: y ! { dg-error "DUMMY attribute conflicts" }

  automatic ! { dg-error "Expected entity-list in AUTOMATIC statement" }
  automatic :: ! { dg-error "Expected entity-list in AUTOMATIC statement" }
  static ! { dg-error "Expected entity-list in STATIC statement" }
  static :: ! { dg-error "Expected entity-list in STATIC statement" }

  integer, automatic :: auto1, auto2
  integer, static :: static1, static2
  integer :: auto3, static3
  automatic :: auto3
  static :: static3

  common /c1/ auto1, auto2 ! { dg-error "COMMON attribute conflicts" }
  common /c2/ static1, static2 ! { dg-error "COMMON attribute conflicts" }
  common /c3/ auto3, static3 ! { dg-error "COMMON attribute conflicts" }

  integer, static :: z ! { dg-error "RESULT attribute conflicts" }
  integer, automatic :: z ! { dg-error "RESULT attribute conflicts" }
  static :: z ! { dg-error "RESULT attribute conflicts" }
  automatic :: z ! { dg-error "RESULT attribute conflicts" }

  integer, static, automatic :: o ! { dg-error "AUTOMATIC attribute conflicts" }

  integer :: a, b, z ! fall-back decls so we don't get "no implicit type"
end
