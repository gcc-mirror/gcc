! { dg-do compile }
! Program to test the initialisation range of enumerators 
! and kind values check

program main
  implicit none
  enum, bind (c)
    enumerator :: pp, qq = 4294967295, rr ! { dg-error "too big for its kind" }
  end enum  ! { dg-error "has no ENUMERATORS" }

  enum, bind (c)
    enumerator :: p , q = 4294967299_8, r  ! { dg-error "Arithmetic overflow" }
  end enum  ! { dg-error "has no ENUMERATORS" }

end program main
