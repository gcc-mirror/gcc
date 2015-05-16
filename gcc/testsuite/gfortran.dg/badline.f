        subroutine foo 
# illegal
# 18 "src/badline.F" 2
# illegal
        end
! { dg-warning "Illegal" "" { target *-*-* } 2 }
! { dg-warning "left but not entered" "" { target *-*-* } 3 }
! { dg-warning "Illegal" "" { target *-*-* } 4 }
