! { dg-do compile }
program p
   print *, [integer :: 1, [integer(8) :: 2, ['3']]] ! { dg-error "Cannot convert" }
end
