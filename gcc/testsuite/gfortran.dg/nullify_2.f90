! { dg-do compile }
! PR fortran/25146
program i
   implicit none
   TYPE (a) t1     ! { dg-error "is being used before" }
   nullify(t1%x)   ! { dg-error "error in NULLIFY" }
end program
