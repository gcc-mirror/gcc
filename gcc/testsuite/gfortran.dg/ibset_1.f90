! { dg-do compile }
program a
   integer :: i = 42
   integer l
   l = ibset(i, -1)  ! { dg-error "must be nonnegative" }
   l = ibset(i, 65)  ! { dg-error "must be less than" }
end program a
