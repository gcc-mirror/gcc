! { dg-do compile }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type :: t
  end type

  interface test
    procedure testAlloc
    procedure testPtr
  end interface

contains

  logical function testAlloc(obj)
    class(t), allocatable :: obj
    testAlloc = .true.
  end function

  logical function testPtr(obj)
    class(t), pointer :: obj
    testPtr = .false.
  end function

end
