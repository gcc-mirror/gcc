! { dg-do compile }
program p
   procedure(g), save :: f ! { dg-error "PROCEDURE attribute conflicts" }
   procedure(g), save :: f ! { dg-error "Duplicate SAVE attribute" }
contains
   subroutine g
   end
end
