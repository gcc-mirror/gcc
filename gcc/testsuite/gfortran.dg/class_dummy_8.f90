! { dg-do compile }
!
! PR fortran/105379
! Type comparison of class containers used to trigger an ICE when one of the
! class containers had a non-constant array spec.
!
! Contributed by Gerhard Steinmetz <gscfq@t-online.de>.

program p
   type t
   end type
contains
   subroutine s1(x)
      class(t) :: x(3)
   end
   subroutine s2(n, x)
      integer :: n
      class(t) :: x(n)
   end
end
