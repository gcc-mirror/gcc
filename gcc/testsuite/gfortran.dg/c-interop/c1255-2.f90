! { dg-do compile }
!
! TS 29113
! C1255 (R1230) If proc-language-binding-spec is specified for a procedure,
! each dummy argument shall be an interoperable procedure (15.3.7) 
! or a variable that is interoperable (15.3.5, 15.3.6), assumed shape, 
! assumed rank, assumed type, of assumed character length, or has the 
! ALLOCATABLE or POINTER attribute. If proc-language-binding-spec is 
! specified for a function, the function result shall be an interoperable 
! scalar variable.
!
! This file contains code that is expected to produce errors.


module m1
   ! type to use for examples below
  type t
    integer :: foo
    real :: bar
  end type
end module

module m2

  interface

    ! dummy is a procedure that is not interoperable
    subroutine s1 (x) bind (c)
      use ISO_C_BINDING
      use m1
      implicit none
      interface
        function x (a, b) bind (c)  ! { dg-error "not C interoperable" }
          use ISO_C_BINDING
          use m1
          integer(C_INT) :: a
          class(t) :: b  !
          integer(C_INT) :: x
        end function
      end interface
    end subroutine

    ! dummy is of a type that is not interoperable
    subroutine s2 (x) bind (c)  ! { dg-error "not C interoperable" }
      use ISO_C_BINDING
      use m1
      implicit none
      class(t) :: x
    end subroutine

    ! dummy is an array that is not of interoperable type and not
    ! assumed-shape or assumed-rank
    subroutine s3 (x) bind (c)  ! { dg-error "not C interoperable" }
      use ISO_C_BINDING
      use m1
      implicit none
      class(t) :: x(3, 3)
    end subroutine

    subroutine s4 (n, x) bind (c)  ! { dg-error "not C interoperable" }
      use ISO_C_BINDING
      use m1
      implicit none
      integer(C_INT) :: n
      class(t) :: x(n)
    end subroutine

    ! This fails with a bogus error even without C binding.
    subroutine s5 (x) bind (c)  ! { dg-error "not C interoperable" }
      use ISO_C_BINDING
      use m1
      implicit none
      class(t) :: x(*)  ! { dg-bogus "not yet been implemented" "pr46991" }
                        ! { dg-bogus "has no IMPLICIT type" "pr46991" { target "*-*-*" } 68 }
    end subroutine

    subroutine s5a (x)
      use ISO_C_BINDING
      use m1
      implicit none
      class(t) :: x(*)  ! { dg-bogus "not yet been implemented" "pr46991" }
                        ! { dg-bogus "has no IMPLICIT type" "pr46991" { target "*-*-*" } 76 }
    end subroutine

    ! function result is not a scalar
    function f (x) bind (c)  ! { dg-error "not C interoperable" }
      use ISO_C_BINDING
      use m1
      implicit none
      integer(C_INT) :: x
      type(t) :: f
    end function

    ! function result is a type that is not interoperable
    function g (x) bind (c)  ! { dg-error "BIND\\(C\\)" }
      use ISO_C_BINDING
      use m1
      implicit none
      integer(C_INT) :: x
      integer(C_INT), allocatable :: g
    end function

  end interface

end module

