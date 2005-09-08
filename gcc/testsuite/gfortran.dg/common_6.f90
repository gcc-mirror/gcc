! { dg-do compile }
! PR 23765 : We used to incorrectly accept common blocks with no symbols
common          ! { dg-error "Syntax error" }
common //       ! { dg-error "Syntax error" }
common /a/      ! { dg-error "Syntax error" }
common /b/x/c/  ! { dg-error "Syntax error" }
common y/d/     ! { dg-error "Syntax error" }
common /e//f/   ! { dg-error "Syntax error" }
common ///g/    ! { dg-error "Syntax error" }
end
