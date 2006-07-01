! { dg-do compile }
! PR 19259 Semicolon cannot start a line
      x=1; y=1;
      x=2;;
      x=3;
      ;                         ! { dg-error "Semicolon at" }
      ;;                        ! { dg-error "Semicolon at" }
 900  ;                         ! { dg-error "Semicolon at" }
      end
