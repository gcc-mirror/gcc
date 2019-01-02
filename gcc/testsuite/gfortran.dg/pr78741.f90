! { dg-do compile }
! PR fortran/78741
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran at t-online.de>
subroutine s(n, x)
   integer :: n
   character(n) :: x
   character, pointer :: z(:)
   x = 'a'
   return
entry g(n, x)           ! { dg-error "is already defined" }
   x = 'b'
contains
   subroutine g         ! { dg-error "(1)" }
      z(1) = x(1:1)
   end
end
