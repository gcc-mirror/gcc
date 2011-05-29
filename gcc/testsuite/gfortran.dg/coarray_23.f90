! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/18918
!
! The example was ICEing before as the tree-decl
! of the type was wrong.
!

  subroutine test
    complex, save :: z[*]
    if (z /= cmplx (0.0, 0.0)) call abort()
  end subroutine test
