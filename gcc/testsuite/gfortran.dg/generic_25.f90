! { dg-do run }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by <wangmianzhi1@linuxmail.org>

  interface test
    procedure testAlloc
    procedure testPtr
  end interface

  integer, allocatable :: a1
  integer, pointer :: a2

  if (.not.test(a1)) call abort()
  if (test(a2)) call abort()
  
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
