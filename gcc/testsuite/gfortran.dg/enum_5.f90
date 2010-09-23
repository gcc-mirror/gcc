! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  integer :: i = 1

  enum, bind (c)
    enumerator :: red, black = i  ! { dg-error "is a variable" }
    enumerator :: blue = 1  
  end enum junk  ! { dg-error "Syntax error" }

  blue = 10  ! { dg-error "Unexpected assignment" }

end program main  ! { dg-error "Expecting END ENUM" }
 ! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
