! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  enum, bind (c)
    enumerator :: red, black = 2     
    enumerator :: blue = 1, red  ! { dg-error "already has basic type" }
  end enum

  enum, bind (c)
    enumerator :: r, b(10) = 2  ! { dg-error "Syntax error" }
    enumerator , save :: g = 1  ! { dg-error "Syntax error" }  
  end  ! { dg-error " END ENUM" } 

end program main  ! { dg-excess-errors "" }
