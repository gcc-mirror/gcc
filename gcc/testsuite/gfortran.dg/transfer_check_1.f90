! { dg-do compile }
! { dg-options -Wsurprising }
! PR fortran/33037
!
print *, transfer('x', 0, 20) ! { dg-warning "has partly undefined result" }
print *, transfer(1_1, 0) ! { dg-warning "has partly undefined result" }
print *, transfer([1_2,2_2], 0)
print *, transfer([1_2,2_2], 0_8) ! { dg-warning "has partly undefined result" }
end
