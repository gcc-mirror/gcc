! { dg-do compile }
program p
   call s(1)
   x = abs(s)  ! { dg-error "must have a numeric type" }
end
subroutine s(n)
   print *, n
end
