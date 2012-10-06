! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by <wangmianzhi1@linuxmail.org>

module a

  interface test
    procedure testAlloc
    procedure testPtr   ! { dg-error "Ambiguous interfaces" }
  end interface

contains

  logical function testAlloc(obj)
    integer, allocatable :: obj
    testAlloc = .true.
  end function
  
  logical function testPtr(obj)
    integer, pointer :: obj
    testPtr = .false.
  end function
  
end

! { dg-final { cleanup-modules "a" } }
