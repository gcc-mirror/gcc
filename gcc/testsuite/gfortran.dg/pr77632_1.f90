! { dg-do run }
program foo
   implicit none
   real, target :: a
   real, pointer :: b => a
   if (associated(b, a) .eqv. .false.) stop 1
end program foo
