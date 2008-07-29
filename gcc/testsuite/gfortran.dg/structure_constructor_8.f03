! { dg-do compile }
! Test for errors when setting private components inside a structure constructor
! or when constructing a private structure.

MODULE privmod
  IMPLICIT NONE

  TYPE :: haspriv_t
    INTEGER :: a
    INTEGER, PRIVATE :: b = 42
  END TYPE haspriv_t

  TYPE :: allpriv_t
    PRIVATE
    INTEGER :: a = 25
  END TYPE allpriv_t

  TYPE, PRIVATE :: ispriv_t
    INTEGER :: x
  END TYPE ispriv_t

CONTAINS
  
  SUBROUTINE testfunc ()
    IMPLICIT NONE
    TYPE(haspriv_t) :: struct1
    TYPE(allpriv_t) :: struct2
    TYPE(ispriv_t) :: struct3

    ! This should succeed from within the module, no error.
    struct1 = haspriv_t (1, 2)
    struct2 = allpriv_t (42)
    struct3 = ispriv_t (42)
  END SUBROUTINE testfunc

END MODULE privmod

PROGRAM test
  USE privmod
  IMPLICIT NONE

  TYPE(haspriv_t) :: struct1
  TYPE(allpriv_t) :: struct2

  ! This should succeed, not giving value to private component
  struct1 = haspriv_t (5)
  struct2 = allpriv_t ()

  ! These should fail
  struct1 = haspriv_t (1, 2) ! { dg-error "is a PRIVATE component" }
  struct1 = haspriv_t (b = 2, a = 1) ! { dg-error "is a PRIVATE component" }

  ! This should fail as all components are private
  struct2 = allpriv_t (5) ! { dg-error "of 'allpriv_t' are PRIVATE" }

  ! This should fail as the type itself is private, and the expression should
  ! be deduced as call to an undefined function.
  WRITE (*,*) ispriv_t (5) ! { dg-error "has no IMPLICIT type" }

END PROGRAM test
! { dg-final { cleanup-modules "privmod" } }
