! { dg-do compile }
program p
   shape(1) = 0      ! { dg-error "expression in variable definition context" }
   shape(1,2) = 0    ! { dg-error "expression in variable definition context" }
   shape(1,2,3) = 0  ! { dg-error "Too many arguments in call" }
   lbound([1]) = 0   ! { dg-error "expression in variable definition context" }
end
