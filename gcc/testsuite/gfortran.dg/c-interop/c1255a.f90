! { dg-do compile }
!
! TS 29113
! C1255a (R1230) A dummy argument of a procedure that has a 
! proc-language-binding-spec shall not have both the OPTIONAL and
! VALUE attributes.
!
! This file contains code that is expected to produce errors.

module m

  interface

    ! This one is OK.
    subroutine s1 (x, y) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x
      integer(C_INT), optional :: y
    end subroutine

    ! This one is OK too.
    subroutine s2 (x, y) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x
      integer(C_INT), value :: y
    end subroutine

    ! This one is bad.
    subroutine s3 (x, y) bind (c)    ! { dg-error "BIND\\(C\\)" }
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x
      integer(C_INT), optional, value :: y
    end subroutine

  end interface

end module
