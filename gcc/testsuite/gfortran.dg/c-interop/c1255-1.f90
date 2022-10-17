! PR92482
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

module m

  interface

    ! dummy is interoperable procedure
    subroutine s1 (x) bind (c)
      use ISO_C_BINDING
      implicit none
      interface
        function x (a, b) bind (c)
          use ISO_C_BINDING
          integer(C_INT) :: a, b
          integer(C_INT) :: x
        end function
      end interface
    end subroutine

    ! dummy is interoperable variable
    subroutine s2 (x) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x
    end subroutine

    ! dummy is assumed-shape array variable
    subroutine s3 (x) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x(:)
    end subroutine

    ! dummy is an assumed-rank array variable
    subroutine s4 (x) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x(..)
    end subroutine

    ! dummy is assumed-type variable
    subroutine s5 (x) bind (c)
      use ISO_C_BINDING
      implicit none
      type(*) :: x
    end subroutine

    ! dummy is assumed length character variable
    subroutine s6 (x) bind (c)
      use ISO_C_BINDING
      implicit none
      character(len=*) :: x
    end subroutine

    ! dummy has allocatable or pointer attribute
    subroutine s7 (x, y) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT), allocatable :: x
      integer(C_INT), pointer :: y
    end subroutine

    ! function result shall be an interoperable scalar variable
    function f (x) bind (c)
      use ISO_C_BINDING
      implicit none
      integer(C_INT) :: x
      integer(C_INT) :: f
    end function

  end interface
end module

