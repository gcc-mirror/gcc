! { dg-do compile }
program foo
   implicit none
   integer :: i = 42
   print *, storage_size(z'1234')     ! { dg-error "cannot be an actual" }
   print *, transfer(z'1234', i)      ! { dg-error "cannot be an actual" }
   print *, transfer(i, z'1234')      ! { dg-error "cannot be an actual" }
   print *, transfer(i, i, z'1234')   ! { dg-error "must be INTEGER" }
end program foo
