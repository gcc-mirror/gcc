! { dg-do compile }
! { dg-options "-std=f2003" }
!
program main
interface
  subroutine foo()
  end 
  integer function bar()
  end 
end interface
contains
  subroutine test()
  end ! { dg-error "Fortran 2008: END statement instead of END SUBROUTINE" }
  end subroutine  ! To silence successive errors
end program

subroutine test2()
contains
  integer function f()
    f = 42
  end ! { dg-error "Fortran 2008: END statement instead of END FUNCTION" }
  end function  ! To silence successive errors
end subroutine test2

