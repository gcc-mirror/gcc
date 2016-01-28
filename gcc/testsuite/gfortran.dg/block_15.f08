! { dg-do compile }
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
! Check fix for PR62536 works as expected.

function f2 (x)
implicit none
  integer f2, x
  block
   block named ! { dg-error "Unclassifiable statement" }
    integer a ! should be SAVEd
    a = a + x ! should increment by y every time
    f2 = a
   end block named ! { dg-error "Syntax error in END BLOCK statement" }
  end block
  return
endfunction

end

