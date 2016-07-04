! { dg-do compile }
! Bug 66575 - Endless compilation on missing end interface 
program p
   procedure(g) :: g ! { dg-error "may not be used as its own interface" }
   procedure(g) ! { dg-error "Syntax error in PROCEDURE statement" }
end
