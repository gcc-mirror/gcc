! { dg-do run }
! Program to test ENUM parsing 

program main
  implicit none
  enum, bind (c)  ! { dg-warning "New in Fortran 2003" }
    enumerator :: red, black
    enumerator blue
  end enum
  if (red /= 0) call abort
end program main
