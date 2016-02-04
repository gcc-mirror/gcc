! { dg-do compile }
!
! PR fortran/62536
! Bad "end block" causes ICE.
subroutine s
   block
   end block named ! { dg-error "Syntax error in END BLOCK statement" }
   return
endsubroutine ! { dg-error "Expecting END BLOCK statement" }
! { dg-prune-output "Unexpected end of file" }
