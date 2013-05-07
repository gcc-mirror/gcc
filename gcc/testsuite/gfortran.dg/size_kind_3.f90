! { dg-do compile }
!
! PR fortran/57142
!
integer :: B(huge(1)+3_8,2_8)
integer(8) :: var1(2), var2, var3

var1 = shape(B) ! { dg-error "SHAPE overflows its kind" }
var2 = size(B) ! { dg-error "SIZE overflows its kind" }
var3 = size(B,dim=1) ! { dg-error "SIZE overflows its kind" }
end
