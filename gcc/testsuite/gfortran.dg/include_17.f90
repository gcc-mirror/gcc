! { dg-do compile }
! { dg-options "-I foo-bar -J foo/bar" }
end 
! { dg-warning "Nonexistent include directory 'foo-bar/'" "" { target *-*-* } 0 }
! { dg-warning "Nonexistent include directory 'foo/bar/'" "" { target *-*-* } 0 }

