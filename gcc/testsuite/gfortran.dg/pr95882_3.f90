! { dg-do compile }

subroutine s(c)
   character(((0)/0)) :: c  ! { dg-error "Division by zero" }
end

