! { dg-do compile }
! PR69497
program p
   block
   do
   end block ! { dg-error "Expecting END DO statement" }
end ! { dg-error "END DO statement expected" }
! { dg-excess-errors "Unexpected end of file" }
