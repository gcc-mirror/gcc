! { dg-do compile }
!
! PR fortran/40881
!
integer :: a(3)
print *, 'Hello'
data a/3*5/ ! { dg-warning "Obsolescent feature: DATA statement at .1. after the first executable statement" }
end
