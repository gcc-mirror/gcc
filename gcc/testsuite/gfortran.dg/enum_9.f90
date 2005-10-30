! { dg-do run }
! { dg-options "-fshort-enums" }
! Program to test enumerations when option -fshort-enums is given

program main
  implicit none
  enum, bind (c)  ! { dg-warning "New in Fortran 2003" }
    enumerator :: red, black = 127
    enumerator blue
  end enum
  if (red /= 0) call abort
  if (black /= 127) call abort
  if (blue /= 128) call abort
end program main
