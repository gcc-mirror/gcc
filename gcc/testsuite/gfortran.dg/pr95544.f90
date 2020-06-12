! { dg-do compile }
! PR fortran/95544 - ICE in gfc_can_put_var_on_stack, at fortran/trans-decl.c:494

program test
   character(:), allocatable :: z
   character(:), pointer     :: p
   character(1), pointer     :: c
   print *, adjustl (null(z)) ! { dg-error "is not permitted as actual argument" }
   print *, adjustr (null(z)) ! { dg-error "is not permitted as actual argument" }
   print *, len     (null(p)) ! { dg-error "is not permitted as actual argument" }
   print *, len     (null(z)) ! { dg-error "is not permitted as actual argument" }
   print *, len_trim(null(c)) ! { dg-error "is not permitted as actual argument" }
   print *, len_trim(null(z)) ! { dg-error "is not permitted as actual argument" }
   print *, trim    (null(z)) ! { dg-error "is not permitted as actual argument" }
end
