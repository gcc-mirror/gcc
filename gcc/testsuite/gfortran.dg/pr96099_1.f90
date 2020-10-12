! { dg-do compile }

program pr96099_1
   implicit class(t) (1) ! { dg-error "Syntax error in IMPLICIT" }
   type t
   end type
end

