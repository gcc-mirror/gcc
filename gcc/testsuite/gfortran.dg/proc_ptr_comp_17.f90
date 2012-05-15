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
 function abc(arg)
  character(len=5),pointer :: abc
  character(len=5),target :: arg
  abc => arg
 end function abc
end module m

use m
 type(t) :: x
 character(len=5) :: str = 'abcde'
 character(len=5), pointer :: strptr
 x%ptr => abc
 print *,x%ptr(str)
 strptr => x%ptr(str)
 if (strptr/='abcde') call abort()
 str = 'fghij'
 if (strptr/='fghij') call abort()
end
