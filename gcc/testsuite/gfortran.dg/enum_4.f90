! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  enum, bind (c)
    enumerator :: red, black = 2     
    enumerator :: blue = 1, red  ! { dg-error "already" }
  end enum

  enum, bind (c)
    enumerator :: r, b(10) = 2  ! { dg-error "cannot be array" }
    enumerator , save :: g = 1  ! { dg-error "cannot have attributes" }  
  end  ! { dg-error " END ENUM" } 

end program main  ! { dg-excess-errors "" }
