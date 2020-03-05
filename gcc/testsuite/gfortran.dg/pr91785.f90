! { dg-do compile }
! PR fortran/91785
! Code contributed by Gerhard Steinmetz
program p
   complex :: a(*)   ! { dg-error "Assumed size array at" }
   real :: b(2)
   b = a%im          ! { dg-error "upper bound in the last dimension" }
end
