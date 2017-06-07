! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by <wangmianzhi1@linuxmail.org>

module a

  interface test
    procedure testAlloc
    procedure testPtr
  end interface

contains

  logical function testAlloc(obj)    ! { dg-error "Ambiguous interfaces" }
    integer, allocatable :: obj
    testAlloc = .true.
  end function
  
  logical function testPtr(obj)      ! { dg-error "Ambiguous interfaces" }
    integer, pointer :: obj
    testPtr = .false.
  end function
  
end
