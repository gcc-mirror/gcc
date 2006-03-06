! { dg-do run }
! Tests the fix for PR19546 in which an ICE would result from
! setting the parent result in a contained procedure. 
! Check that parent alternate entry results can be referenced.
! 
function f()
  integer :: f, g
  f = 42
  call sub1 ()
  if (f.eq.1) f = 2
  return
entry g()
  g = 99
  call sub2 ()
  if (g.eq.77) g = 33
contains
  subroutine sub1
    if (f.eq.42) f = 1
  end subroutine sub1
  subroutine sub2
    if (g.eq.99) g = g - 22
  end subroutine sub2
end function f

  integer, external :: f, g
  if (f ().ne.2) call abort ()
  if (g ().ne.33) call abort ()
end
