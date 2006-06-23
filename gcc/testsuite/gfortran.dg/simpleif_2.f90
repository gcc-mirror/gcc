! { dg-do compile }
! PR 27981
program a
   real x
   real, pointer :: y
   if (.true.) x = 12345678901 ! { dg-error "Integer too big" }
end program a
