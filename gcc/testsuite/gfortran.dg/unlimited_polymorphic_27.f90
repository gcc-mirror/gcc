! { dg-do compile }
!
! PR 78800: [OOP] ICE in compare_parameter, at fortran/interface.c:2246
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

program p
   type t
   end type
   class(*) :: z  ! { dg-error "must be dummy, allocatable or pointer" }
   call s(z)
contains
   subroutine s(x)
      type(t) :: x
   end
end
