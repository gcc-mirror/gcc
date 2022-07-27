! { dg-do compile }
! { dg-options "-I gfortran.log" }
! { dg-error "'gfortran.log' is not a directory" "" { target *-*-* } 0 }
! { dg-prune-output "compilation terminated." }
end 

