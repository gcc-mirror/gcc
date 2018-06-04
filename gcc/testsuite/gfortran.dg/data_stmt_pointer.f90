! { dg-do run }
program foo
   real, pointer :: p
   real, save, target :: x = 42
   data p / x /
   if (p /= 42) stop 1
   call bar
end program foo

subroutine bar
   type bah
     integer, pointer :: p
   end type bah
   type(bah) a
   integer, save, target :: i = 42
   data a%p / i /
   if (a%p /= 42) stop 2
end subroutine

