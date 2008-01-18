! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  integer :: i = 1

  enum, bind (c)
    enumerator :: sun, mon = 2    
    i = 2  ! { dg-error "Unexpected" }  
    enumerator :: wed = 1    
  end enum       

  i = 1

  enum, bind (c)  ! { dg-error "Unexpected" } 
    enumerator :: red, black = 2  ! { dg-error "ENUM definition statement expected" }
    enumerator :: blue = 1  ! { dg-error "ENUM definition statement expected" }
  end enum  ! { dg-error "Expecting END PROGRAM" }

end program main        
