! { dg-do compile }
! { dg-options "-J foobar/foo" }
program main
end program main
! { dg-warning "Nonexistent include directory" "" { target *-*-* } 0 }
