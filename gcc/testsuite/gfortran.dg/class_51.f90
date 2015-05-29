! { dg-do compile }
! { dg-options "-fdump-tree-original" }
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
    Call sub(x)
    Print *,x%c
    if (x%c /= 3) call abort ()
  Contains
    Subroutine sub(p)
      Class(t),Pointer,Intent(In) :: p
      p%c = 3
    End Subroutine
  End Program

! { dg-final { scan-tree-dump-times "sub \\(&class" 1 "original" } }
