! { dg-do compile }
! One of the tests of the patch for PR30068.
! Taken from comp.lang.fortran 3rd December 2006.
!
! Although the generic procedure is not referenced and it would
! normally be permissible for it to be ambiguous, the USE, ONLY
! statement is effectively a reference and is invalid.
!
module mod1
   interface generic
      subroutine foo(a)
         real :: a
      end subroutine
   end interface generic
end module  mod1

module mod2
   interface generic
      subroutine bar(a)
         real :: a
      end subroutine
   end interface generic
end module  mod2

program main
  use mod1, only: generic   ! { dg-warning "has ambiguous interfaces" }
  use mod2
end program main
