! { dg-do compile }
! PR69497
program p
   block
   do
   end block ! { dg-error "Expecting END DO statement" }
end ! { dg-error "END DO statement expected" }
! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
