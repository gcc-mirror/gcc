! { dg-do run }
! PR26766 Recursive I/O with internal units
! Test case derived from example in PR
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program pr26766
  implicit none
  character (len=8) :: str, tmp
  write (str, '(a)')  bar (1234)
  if (str.ne."abcd") STOP 1
  str = "wxyz"
  write (str, '(2a4)') foo (1), bar (1)
  if (str.ne."abcdabcd") STOP 2

contains

  function foo (i) result (s)
    integer, intent(in) :: i
    character (len=4)   :: s, t
    if (i < 0) then
       s = "1234"
    else
       ! Internal I/O, allowed recursive in f2003, see section 9.11
       write (s, '(a)') "abcd"
    end if
  end function foo
  
  function bar (i) result (s)
    integer, intent(in) :: i
    character (len=4)   :: s, t
    if (i < 0) then
      s = "4567"
    else
      write (s, '(a)') foo(i)
    end if
  end function bar

end program pr26766


