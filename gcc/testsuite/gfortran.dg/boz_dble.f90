! { dg-do run }
program foo
   double precision x
   x = dble(z"400921FB54411744");
   if (x /= 3.1415926535_8) stop 1
end
