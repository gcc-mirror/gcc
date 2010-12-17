! { dg-do compile }
! Program to test ENUM parsing errors 

program main
  implicit none
  enum, bind (c)
    enumerator :: red, black
    integer :: x  ! { dg-error "Unexpected data declaration" }
    enumerator blue = 1  ! { dg-error "Syntax error in ENUMERATOR definition" }
  end enum

  red = 42 ! { dg-error "variable definition context" }

  enumerator :: sun  ! { dg-error "ENUM" }
end program main
