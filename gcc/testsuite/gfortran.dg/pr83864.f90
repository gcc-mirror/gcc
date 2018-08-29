! { dg-do run }
! PR fortran/83864
!
! Derived from PR by Contributed by Gerhard Steinmetz <gscfq@t-online.de>
!
program p
  implicit none
  type t
     character :: c(3) = transfer('abc','z',3)
  end type t
  type(t) :: x
  if (any (x%c /= ["a", "b", "c"])) STOP 1
end
