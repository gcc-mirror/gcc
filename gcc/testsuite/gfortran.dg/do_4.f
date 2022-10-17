! { dg-do compile }
! Verify that the loop not terminated on an action-stmt is correctly rejected
       do10i=1,20
       if(i.eq.5)then
          goto 10
 10    endif           ! { dg-error "is within another block" }
       end             ! { dg-error "END DO statement expected" }
                       ! { dg-warning "Fortran 2018 deleted feature: DO termination statement which is not END DO or CONTINUE" "" { target "*-*-*" } 6 }
! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }

