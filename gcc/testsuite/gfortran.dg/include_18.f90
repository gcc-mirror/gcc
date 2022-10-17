! { dg-do compile }
! { dg-options "-I nothere -J neither/here -Wmissing-include-dirs" }
end 
! { dg-warning "Nonexistent include directory 'nothere'" "" { target *-*-* } 0 }
! { dg-warning "Nonexistent include directory 'neither/here'" "" { target *-*-* } 0 }
