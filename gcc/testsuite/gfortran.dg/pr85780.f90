! { dg-do compile }
! { dg-options "-fmax-errors=1" }
! PR fortran/85780
subroutine s(*) bind(c)    ! { dg-error "Alternate return dummy argument" }
end
! { dg-prune-output "compilation terminated" } 
