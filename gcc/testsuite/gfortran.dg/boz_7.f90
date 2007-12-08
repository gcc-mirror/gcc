! { dg-do compile }
! { dg-options "-std=f95 -pedantic" }
!
! PR fortran/34342
!
! Some BOZ extensions where not diagnosed
!
integer :: k, m
integer :: j = z'000abc' ! { dg-error "BOZ used outside a DATA statement" }
data k/x'0003'/ ! { dg-error "uses non-standard syntax" }
data m/'0003'z/ ! { dg-error "uses non-standard postfix syntax" }
end
