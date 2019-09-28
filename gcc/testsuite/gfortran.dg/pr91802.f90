! { dg-do compile }
! { dg-options "-fcoarray=single" }
! Code contributed by Gerhard Steinmetz
! PR fortran/91802
module m
   real :: x
   dimension ::   x(1,2,1,2,1,2,1,2)
   codimension :: x[1,2,1,2,1,2,1,*] ! { dg-error "exceeds 15" }
end
