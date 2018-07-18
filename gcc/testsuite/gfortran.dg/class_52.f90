! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/52270
!
! From IR F08/0073 by Malcolm Cohen
!

  Program m013
    Type t
      Real c
    End Type
    Type(t),Target :: x
    Call sub(x) ! { dg-error "Fortran 2008: Non-pointer actual argument" }
    Print *,x%c
    if (x%c /= 3) STOP 1
  Contains
    Subroutine sub(p)
      Class(t),Pointer,Intent(In) :: p
      p%c = 3
    End Subroutine
  End Program

