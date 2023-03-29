! { dg-do compile }

subroutine s1(x)
   integer, intent(out) :: x
end
subroutine s2(z)
   integer, value :: z
   call s1(z)
end
