! { dg-do compile }
!
! PR fortran/49397
!
! Valid per IR F08/0060 and F2008Corr2, C729
!
Program m5
  Print *,f()
Contains
  Subroutine s
    Procedure(Real),Pointer :: p
    Print *,g()
    p => f                           ! (1)
    Print *,p()
    p => g                           ! (2)
    Print *,p()
  End Subroutine
End Program
Function f()
  f = 1
End Function
Function g()
  g = 2
End Function
