! { dg-do compile }
! { dg-options "-Werror -fmax-errors=1" }
100   ! { dg-warning "empty statement" }
end
! { dg-error "count reached limit" "" { target *-*-* } 0 }
