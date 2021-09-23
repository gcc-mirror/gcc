! PR 101320
! { dg-do compile }
!
! TS 29113
! C516  The ALLOCATABLE or POINTER attribute shall not be specified for
! a default-initialized dummy argument of a procedure that has a 
! proc-language-binding-spec.
!
! This file contains code that is expected to produce errors.

module m1

  type, bind(c) :: t1
    integer :: a
    integer :: b
  end type


  type, bind(c) :: t2
    integer :: a = 0
    integer :: b = -1
  end type

end module

module m2

  interface

    ! good, no default initialization, no pointer/allocatable attribute
    subroutine s1a (x) bind (c)
      use m1
      type(t1), optional :: x
    end subroutine

    ! good, no default initialization
    subroutine s1b (x) bind (c)
      use m1
      type(t1), allocatable, optional :: x
    end subroutine

    ! good, no default initialization
    subroutine s1c (x) bind (c)
      use m1
      type(t1), pointer, optional :: x
    end subroutine

    ! good, default initialization but no pointer/allocatable attribute
    subroutine s2a (x) bind (c)
      use m1
      type(t2), optional :: x
    end subroutine

    ! bad, default initialization + allocatable
    subroutine s2b (x) bind (c)  ! { dg-error "BIND\\(C\\)" "pr101320" { xfail *-*-* } }
      use m1
      type(t2), allocatable, optional :: x
    end subroutine

    ! bad, default initialization + pointer
    subroutine s2c (x) bind (c)  ! { dg-error "BIND\\(C\\)" "pr101320" { xfail *-*-* } }
      use m1
      type(t2), pointer, optional :: x
    end subroutine

  end interface
end module
