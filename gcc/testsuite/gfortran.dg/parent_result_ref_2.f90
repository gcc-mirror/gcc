! { dg-do run }
! Tests the fix for PR19546 in which an ICE would result from
! setting the parent result in a contained procedure. 
! This case tests character results.
! 
function f()
  character(4) :: f
  f = "efgh"
  call sub ()
  if (f.eq."iklm") f = "abcd"
  call sub ()
contains
  subroutine sub
    f = "wxyz"
    if (f.eq."efgh") f = "iklm"
  end subroutine sub
end function f

function g()              ! { dg-warning "is obsolescent in fortran 95" }
  character(*) :: g
  g = "efgh"
  call sub ()
  if (g.eq."iklm") g = "ABCD"
  call sub ()
contains
  subroutine sub
    g = "WXYZ"
    if (g.eq."efgh") g = "iklm"
  end subroutine sub
end function g

  character(4), external :: f, g
  if (f ().ne."wxyz") call abort ()
  if (g ().ne."WXYZ") call abort ()
end
