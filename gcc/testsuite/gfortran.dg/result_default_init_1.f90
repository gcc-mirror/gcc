! { dg-do run }
! { dg-options "-O" }
! Test the fix for PR29216 in which function results did not
! get default initialization.
! Contributed by Stephan Kramer  <stephan.kramer@imperial.ac.uk>  
!
  type A
    integer, pointer:: p => null ()
    integer:: i=3
  end type A
  type(A):: x,y
  if (associated(x%p) .or. x%i /= 3) call abort ()
  x=f()
  if (associated(x%p) .or. x%i /= 3) call abort ()
  x=g()
  if (associated(x%p) .or. x%i /= 3) call abort ()
contains
  function f() result (fr)
    type(A):: fr
    if (associated(fr%p) .or. fr%i /= 3) call abort ()
  end function f
  function g()
    type(A):: g
    if (associated(g%p) .or. g%i /= 3) call abort ()
  end function g
end
