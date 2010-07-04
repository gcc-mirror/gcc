! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR 19259 Semicolon cannot start a line (in F2003)
      x=1; y=1;
      x=2;;
      x=3;
      ;                         ! { dg-error "Fortran 2008: Semicolon at" }
      ;;                        ! { dg-error "Fortran 2008: Semicolon at" }
 900  ;                         ! { dg-error "Semicolon at" }
      end
