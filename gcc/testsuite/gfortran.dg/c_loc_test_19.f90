! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/50269
!
Program gf
   Use iso_c_binding
   Real( c_double ), Dimension( 1:10 ), Target :: a
   Call test( a )
Contains
   Subroutine test( aa )
     Real( c_double ), Dimension( : ), Target :: aa
     Type( c_ptr ), Pointer :: b
     b = c_loc( aa( 1 ) )  ! was rejected before.
     b = c_loc( aa ) ! { dg-error "Fortran 2008: Array of interoperable type at .1. to C_LOC which is nonallocatable and neither assumed size nor explicit size" }
   End Subroutine test
End Program gf
