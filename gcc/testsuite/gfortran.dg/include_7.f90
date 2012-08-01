! { dg-do compile }
! { dg-options "-I nothere" }
! { dg-warning "Nonexistent include directory" "missing directory" { target *-*-* } 0 }
end 

