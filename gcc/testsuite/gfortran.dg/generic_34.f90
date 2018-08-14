! { dg-do compile }
!
! PR 86116: [6/7/8/9 Regression] Ambiguous generic interface not recognised
!
! Contributed by martin <mscfd@gmx.net>

module mod

   type :: t
   end type t

   interface sub
      module procedure s1
      module procedure s2
   end interface

contains

   subroutine s1(x)  ! { dg-error "Ambiguous interfaces in generic interface" }
      type(t) :: x
   end subroutine

   subroutine s2(x)  ! { dg-error "Ambiguous interfaces in generic interface" }
      class(*), allocatable :: x
   end subroutine

end
