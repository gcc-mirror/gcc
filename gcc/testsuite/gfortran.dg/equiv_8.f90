! { dg-do compile }
!
! PR fortran/41755
!
      common /uno/ aa
      equivalence (aa,aaaaa)   (bb,cc) ! { dg-error "Expecting a comma in EQUIVALENCE" }
      end
