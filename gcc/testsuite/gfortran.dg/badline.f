	subroutine foo 
# 18 "src/badline.F" 2
	end
! { dg-warning "left but not entered" "" { target *-*-* } 2 }
