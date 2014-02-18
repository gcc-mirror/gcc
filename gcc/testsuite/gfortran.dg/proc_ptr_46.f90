! { dg-do compile }
!
! PR fortran/49397
!
! Invalid per IR F08/0060 and F2008Corr2, C729
!

!  Print *,f() ! << Valid when uncommented
Contains
  Subroutine s
    Procedure(Real),Pointer :: p
    p => f  ! { dg-error "Procedure pointer target 'f' at .1. must be either an intrinsic, host or use associated, referenced or have the EXTERNAL attribute" }
  End Subroutine
End
