! { dg-do run }
! { dg-options "-fallow-invalid-boz -w" }
program foo
   integer i
   i = float(z'1234')
   if (i /= 4660.0) stop 1
end program foo
