! { dg-do run }
!
! PR 41106: [F03] Procedure Pointers with CHARACTER results
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
 type :: t
 procedure(character(len=5)), pointer, nopass :: ptr
 end type
contains
 function abc()
  character(len=5) :: abc
  abc = 'abcde'
 end function abc
end module m

use m
 type(t) :: x
 character(len=5) :: str
 x%ptr => abc
 print *,x%ptr()
 str = x%ptr()
 if (str/='abcde') call abort()
end
