! { dg-do compile }
! { dg-options "-O2" }
! PR fortran/92874
program p
   call s('a')
   call s('abc')
end
subroutine s(x)
   character(*) :: x
   print *, (x(1:1) == x(1:))
end
