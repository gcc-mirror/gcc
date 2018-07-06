! { dg-do run }
!
! PR 41106: [F03] Procedure Pointers with CHARACTER results
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

character(len=5) :: str
procedure(character(len=5)), pointer :: pp
pp => abc
print *,pp()
str = pp()
if (str/='abcde') STOP 1
contains
 function abc()
  character(len=5) :: abc
  abc = 'abcde'
 end function abc
end

