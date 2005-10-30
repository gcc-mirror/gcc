! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  enum, bind (c)  ! { dg-warning "New in Fortran 2003" } 
    enumerator :: red, black = 2.2  ! { dg-error "initialized with integer expression" }
    enumerator :: blue = "x"  ! { dg-error "initialized with integer expression" }
  end enum  ! { dg-error "has no ENUMERATORS" }

end program main
