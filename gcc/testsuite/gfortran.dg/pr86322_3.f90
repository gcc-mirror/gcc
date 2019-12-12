! { dg-do run }
program bar
   type a
     integer, pointer :: i
   end type a
   type b
     type(a) :: j
   end type b
   integer, target, save :: k = 42
   type(b) x
   data x%j%i/k/
   if (x%j%i /= 42) stop 1
end program bar
