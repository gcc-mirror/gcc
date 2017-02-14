! { dg-do compile }
program p
   use iso_c_binding
   character(len=1, kind=c_char), parameter :: z(2) = 'z'
   print *, sizeof(z(3))      ! { dg-warning "is out of bounds" }
   print *, c_sizeof(z(3))    ! { dg-warning "is out of bounds" }
end
