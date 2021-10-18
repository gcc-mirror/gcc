! PR92482
! { dg-do compile}
!
! TS 29113
! 8.7 Interoperability of procedures and procedure interfaces
! 
! If a dummy argument in an interoperable interface is of type
! CHARACTER and is allocatable or a pointer, its character length shall
! be deferred.
!
! This test checks that this error is diagnosed and is supposed to fail.

module m
  use iso_c_binding

  interface

    ! These are supposed to be OK
    subroutine good1 (x, n) bind (c)
      use iso_c_binding
      character (kind=C_CHAR, len=:), allocatable :: x
      integer(C_INT), value :: n
    end subroutine
    subroutine good2 (x, n) bind (c)
      use iso_c_binding
      character (kind=C_CHAR, len=:), pointer :: x
      integer(C_INT), value :: n
    end subroutine

    ! These are supposed to fail.
    subroutine bad1 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR, len=*), allocatable :: x
      integer(C_INT), value :: n
    end subroutine
    subroutine bad2 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR, len=*), pointer :: x
      integer(C_INT), value :: n
    end subroutine

    subroutine bad3 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR, len=80), allocatable :: x
      integer(C_INT), value :: n
    end subroutine
    subroutine bad4 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR, len=80), pointer :: x
      integer(C_INT), value :: n
    end subroutine

    subroutine bad5 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR, len=1), allocatable :: x
      integer(C_INT), value :: n
    end subroutine
    subroutine bad6 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR, len=1), pointer :: x
      integer(C_INT), value :: n
    end subroutine

    subroutine bad7 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR), allocatable :: x
      integer(C_INT), value :: n
    end subroutine
    subroutine bad8 (x, n) bind (c)  ! { dg-error "must have deferred length" }
      use iso_c_binding
      character (kind=C_CHAR), pointer :: x
      integer(C_INT), value :: n
    end subroutine
  end interface

end module
