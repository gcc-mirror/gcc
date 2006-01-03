! { dg-do compile }
! PR fortran/25101  -- Stride must be nonzero.
program forall_2
   integer :: a(10),j(2),i
   forall(i=1:2:0)  ! { dg-error "stride expression at" }
     a(i)=1
   end forall
end program forall_2

