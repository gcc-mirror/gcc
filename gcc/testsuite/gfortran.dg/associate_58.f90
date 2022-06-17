! { dg-do compile }
!
! PR fortran/104570
! The following used to cause an ICE because the string length
! evaluation of the (y) expression was not prepared to handle
! a non-scalar expression.

program p
   character(:), allocatable :: x(:)
   x = ['abc']
   call s
contains
   subroutine s
      associate (y => x)
         associate (z => (y))
            print *, z
         end associate
      end associate
   end
end

