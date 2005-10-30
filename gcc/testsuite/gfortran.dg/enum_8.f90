! { dg-do compile }
! Program to test the initialisation range of enumerators 
! and kind values check

program main
  implicit none
  enum, bind (c)  ! { dg-warning "New in Fortran 2003" }
    enumerator :: pp , qq = 4294967295, rr  ! { dg-error "not initialized with integer" }
  end enum  ! { dg-error "has no ENUMERATORS" }

  enum, bind (c)  ! { dg-warning "New in Fortran 2003" }
    enumerator :: p , q = 4294967299_8, r  ! { dg-error "Arithmetic overflow" }
  end enum  ! { dg-error "has no ENUMERATORS" }

end program main
