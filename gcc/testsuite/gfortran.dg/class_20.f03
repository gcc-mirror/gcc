! { dg-do compile }
!
! PR 44044: [OOP] SELECT TYPE with class-valued function
! comment #1
!
! Note: All three error messages are being checked for double occurrence,
!       using the trick from PR 30612.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>


implicit none

type :: t
end type

type :: s
  sequence
end type

contains

  function fun()  ! { dg-bogus "must be dummy, allocatable or pointer.*must be dummy, allocatable or pointer" }
    class(t) :: fun
  end function
 
  function fun2()  ! { dg-bogus "cannot have a deferred shape.*cannot have a deferred shape" }
    integer,dimension(:) :: fun2
  end function
 
  function fun3() result(res)  ! { dg-bogus "is not extensible.*is not extensible" }
    class(s),pointer :: res
  end function

end


! { dg-error "must be dummy, allocatable or pointer" "" { target *-*-* } 23 }
! { dg-error "cannot have a deferred shape"          "" { target *-*-* } 27 }
! { dg-error "is not extensible"                     "" { target *-*-* } 31 }
