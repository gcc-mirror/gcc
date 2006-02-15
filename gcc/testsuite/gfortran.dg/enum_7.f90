! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none

  enum, bind (c)
    enumerator :: sun, mon = 2    
    enum, bind (c)  ! { dg-error "Unexpected" }
      enumerator :: apple, mango
    end enum  
    enumerator :: wed = 1  ! { dg-error "ENUM definition statement expected" }  
  end enum  ! { dg-error "Expecting END PROGRAM" }  

end program main        
