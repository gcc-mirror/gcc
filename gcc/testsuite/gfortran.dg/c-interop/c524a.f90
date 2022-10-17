! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }
!
! TS 29113
! C524a A coarray shall not be a dummy argument of a procedure that has 
! a proc-language-binding-spec.
!
! This file contains code that is expected to produce errors.

module m

  interface

    ! No C binding, this should be OK.
    subroutine s1 (x)
      use ISO_C_BINDING
      implicit none
      integer(C_INT), codimension[*] :: x(:,:)
    end subroutine

    ! This one is bad.
    subroutine s2 (x) bind (c)  ! { dg-error "BIND\\(C\\)" }
      use ISO_C_BINDING
      implicit none
      integer(C_INT), codimension[*] :: x(:,:)
    end subroutine

  end interface
end module

