! { dg-do run }
module m
   integer :: name_in_module = 123
end module

program foo

   use m, name_in_program => name_in_module
   namelist /nl/ name_in_program

   if (name_in_program /= 123) stop 1

   open(unit=10, file='fort.10', status='replace')
   write(10,nl)
   close(10)

   name_in_program = 42
   if (name_in_program /= 42) stop 2

   open(unit=10, file='fort.10', status='old')
   read(10,nl)
   if (name_in_program /= 123) stop 3
   close(10)

   call bar

   contains

      subroutine bar
         integer name_in_program
         namelist /nl/ name_in_program
         name_in_program = 0
         open(unit=10, file='fort.10', status='old')
         read(10,nl)
         if (name_in_program /= 123) stop 4
         close(10,status='delete')
      end subroutine bar

end
