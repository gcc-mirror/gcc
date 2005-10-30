! Program to test parsing of ENUM in different program units

program main
  implicit none
  interface 
    subroutine sub1
    end subroutine sub1
  end interface
  integer :: i = 55

  enum , bind (c)
    enumerator :: a , b=5
    enumerator c, d
  end enum

  call sub
  call sub1
  i = fun() 

contains

  subroutine sub
    enum, bind(c)
      enumerator :: p = b, q = 10 + 50
      enumerator r, s
    end enum
  end subroutine sub

  function fun()
  integer :: fun
  enum, bind (c)
    enumerator :: red, yellow = 23
    enumerator :: blue 
    enumerator :: green
  end enum
  fun = 1
  end function fun
end program main

subroutine sub1
  implicit none
  enum, bind(c)  
    enumerator x , y
    enumerator :: z = 100
  end enum
end subroutine sub1
