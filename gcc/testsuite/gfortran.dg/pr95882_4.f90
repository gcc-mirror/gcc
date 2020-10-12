! { dg-do compile }

program p
   character(((0)/0)) :: c  ! { dg-error "Division by zero" }
   common /x/ c
end

