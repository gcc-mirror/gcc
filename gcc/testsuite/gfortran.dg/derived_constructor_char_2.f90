! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/44857
!
!

  Type :: t
    character (len=5) :: txt(2)
  End Type
  character (len=5) :: str(2) = [ "12345", "67890" ]
  Type (t) :: tt = t( [str] ) ! { dg-error "does not reduce to a constant" }
End
