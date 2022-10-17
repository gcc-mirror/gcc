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

    ! First test versions with optional attributes on the argument.
    ! TS29113 removed the constraint disallowing optional arguments
    ! that previously used to be in C516.

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
    subroutine s2b (x) bind (c)  ! { dg-error "BIND\\(C\\)" }
      use m1
      type(t2), allocatable, optional :: x
    end subroutine

    ! bad, default initialization + pointer
    subroutine s2c (x) bind (c)  ! { dg-error "BIND\\(C\\)" }
      use m1
      type(t2), pointer, optional :: x
    end subroutine

    ! Now do all the same tests without the optional attribute.

    ! good, no default initialization, no pointer/allocatable attribute
    subroutine s3a (x) bind (c)
      use m1
      type(t1) :: x
    end subroutine

    ! good, no default initialization
    subroutine s3b (x) bind (c)
      use m1
      type(t1), allocatable :: x
    end subroutine

    ! good, no default initialization
    subroutine s3c (x) bind (c)
      use m1
      type(t1), pointer :: x
    end subroutine

    ! good, default initialization but no pointer/allocatable attribute
    subroutine s4a (x) bind (c)
      use m1
      type(t2) :: x
    end subroutine

    ! bad, default initialization + allocatable
    subroutine s4b (x) bind (c)  ! { dg-error "BIND\\(C\\)" }
      use m1
      type(t2), allocatable :: x
    end subroutine

    ! bad, default initialization + pointer
    subroutine s4c (x) bind (c)  ! { dg-error "BIND\\(C\\)" }
      use m1
      type(t2), pointer :: x
    end subroutine

  end interface
end module
