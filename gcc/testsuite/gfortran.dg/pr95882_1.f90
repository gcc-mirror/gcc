! { dg-do compile }

module m
   type t
      character(((0)/0)) :: c  ! { dg-error "Division by zero" }
   end type
end

