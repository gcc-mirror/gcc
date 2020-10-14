! { dg-do compile }

program pr96099_2
   integer n1
   parameter (n1 = 1)
   implicit class(t) (n1) ! { dg-error "Syntax error in IMPLICIT" }
   type t
   end type
end
