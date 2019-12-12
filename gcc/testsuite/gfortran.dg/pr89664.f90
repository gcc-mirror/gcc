! { dg-do compile }
! { dg-options "-Ofast" }

subroutine s (x)
   real :: x
   call sub (x)
end
subroutine sub (x)
   real :: x, y
   logical :: a, b
   real :: f1, f2, f3, f4
   y = f1()
   a = .false.
   if ( f2() > f3() ) a = .true.
   b = .false.
   if ( f2() > f4() ) b = .true.
   if ( a ) then
      x = 1.0
   else if ( b ) then
      x = 1.0/y**2
   else
      x = 1.0/y - y**2
   end if
end
