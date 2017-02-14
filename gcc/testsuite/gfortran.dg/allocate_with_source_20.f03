! { dg-do run }

! Check that PR72698 is fixed.
! Contributed by Gerhard Steinmetz

module m
contains
   integer function f()
      f = 4
   end
end
program p
   use m
   character(3), parameter :: c = 'abc'
   character(:), allocatable :: z
   allocate (z, source=repeat(c(2:1), f()))
   if (len(z) /= 0) call abort()
   if (z /= "") call abort()
end


