! { dg-do compile }
!
! PR fortran/104126
!
! Contributed by G. Steinmetz 
!
program p
   use iso_c_binding, only: c_char
   character(len=:,kind=c_char), pointer :: d
   call s(null(d))
   call s(null())
contains
   subroutine s(x) bind(c)
      character(len=:, kind=c_char), pointer, intent(in) :: x
   end
end
