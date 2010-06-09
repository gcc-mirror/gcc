! { dg-do compile }
program a
   integer :: i = 42
   logical l
   l = btest(i, -1)  ! { dg-error "must be nonnegative" }
   l = btest(i, 65)  ! { dg-error "must be less than" }
end program a
