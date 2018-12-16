! { dg-do compile }
program foo
   print *, [integer :: 1, [integer(8) :: 2, '3']] ! { dg-error "Can\'t convert" }
end program foo
