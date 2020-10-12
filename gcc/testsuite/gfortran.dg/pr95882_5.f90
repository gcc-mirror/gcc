! { dg-do compile }

program p
   character(0/(0)) :: c = '123456789'  ! { dg-error "Division by zero" }
   common c
end
