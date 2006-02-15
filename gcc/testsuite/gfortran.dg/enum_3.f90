! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  enum, bind (c)
    enumerator :: red, black = 2.2  ! { dg-error "initialized with integer expression" }
    enumerator :: blue = "x"  ! { dg-error "initialized with integer expression" }
  end enum  ! { dg-error "has no ENUMERATORS" }

end program main
