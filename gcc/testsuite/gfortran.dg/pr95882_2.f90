! { dg-do compile }

module m
   character(0/(0)) :: c = '123456789'  ! { dg-error "Division by zero" }
end

