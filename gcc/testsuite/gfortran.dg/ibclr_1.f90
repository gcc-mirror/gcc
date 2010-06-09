! { dg-do compile }
program a
   integer :: i = 42
   integer l
   l = ibclr(i, -1)  ! { dg-error "must be nonnegative" }
   l = ibclr(i, 65)  ! { dg-error "must be less than" }
end program a
