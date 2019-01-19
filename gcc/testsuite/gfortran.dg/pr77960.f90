! { dg-do compile }
! PR fortran/77960
   procedure(g), pointer :: f
   f => g
   read(99) f                 ! { dg-error "Expecting variable" }
contains
   function g() result(z)
      integer :: z(2)
      z = 1
   end
end

subroutine bar(x)
   integer, external :: x
   read(*,*) x                ! { dg-error "Expecting variable" }
end subroutine
