! { dg-do compile }
subroutine s(x)
   class(*), allocatable :: x
   x = ''
end
