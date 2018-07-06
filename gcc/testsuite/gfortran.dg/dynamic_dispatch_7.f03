! { dg-do run }
! Test the fix for PR43291, which was a regression that caused
! incorrect type mismatch errors at line 46. In the course of
! fixing the PR, it was noted that the dynamic dispatch of the
! final typebound call was not occurring - hence the dg-do run.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>
!
module m1
  type :: t1
  contains
    procedure :: sizeof
  end type
contains
  integer function sizeof(a)
    class(t1) :: a
    sizeof = 1
  end function sizeof
end module
	
module m2
  use m1
  type, extends(t1) :: t2
  contains
    procedure :: sizeof => sizeof2
  end type
contains
  integer function sizeof2(a)
    class(t2) :: a
    sizeof2 = 2
  end function
end module

module m3
  use m2
  type :: t3
  class(t1), pointer :: a
  contains
    procedure :: sizeof => sizeof3
  end type
contains
  integer function sizeof3(a)
    class(t3) :: a
    sizeof3 = a%a%sizeof()
  end function
end module

  use m1
  use m2
  use m3
  type(t1), target :: x
  type(t2), target :: y
  type(t3) :: z
  z%a => x
  if ((z%sizeof() .ne. 1) .or. (z%a%sizeof() .ne. 1)) STOP 1
  z%a => y
  if ((z%sizeof() .ne. 2) .or. (z%a%sizeof() .ne. 2)) STOP 2
end
 	
