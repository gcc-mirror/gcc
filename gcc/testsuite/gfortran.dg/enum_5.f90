! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  integer :: i = 1

  enum, bind (c)
    enumerator :: red, black = i  ! { dg-error "is a variable" }
    enumerator :: blue = 1  
  end enum junk  ! { dg-error "Syntax error" }

  blue = 10  ! { dg-error "Expected VARIABLE" }

end program main  ! { dg-excess-errors "" }
