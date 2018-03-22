! { dg-do run }
! Tests the fix for PR19546 in which an ICE would result from
! setting the parent result in a contained procedure. 
! From the testcase of Francois-Xavier Coudert/Tobias Schlueter
! 
function f()
  integer :: f
  f = 42
  call sub ()
  if (f.eq.1) f = f + 1
contains
  subroutine sub
    if (f.eq.42) f = f - 41
  end subroutine sub
end function f

  integer, external :: f
  if (f ().ne.2) STOP 1
end
