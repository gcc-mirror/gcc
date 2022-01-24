! { dg-do compile }
! { dg-options "-I gfortran.log" }
! { dg-warning "Include directory 'gfortran.log/': Not a directory" "" { target *-*-* } 0 }
! { dg-prune-output "compilation terminated." }
end 

