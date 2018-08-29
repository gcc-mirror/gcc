! { dg-do run }
!
! PR 41106: [F03] Procedure Pointers with CHARACTER results
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m
 type :: t
  procedure(abc), pointer, nopass :: ptr
 end type
contains
 function abc(i)
  integer :: i
  character(len=i) :: abc
  abc = 'abcde'
 end function abc
end module m

use m
 type(t) :: x
 character(len=4) :: str
 x%ptr => abc
 print *,x%ptr(4)
 if (x%ptr(4)/='abcd') STOP 1
 str = x%ptr(3)
 if (str/='abc') STOP 1
end
