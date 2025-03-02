! { dg-do compile }
! { dg-additional-options "-std=f2018" }
!
! PR fortran/101577
!
! Contributed by Tobias Burnus

type, bind(C) :: t ! { dg-error "has no components" }
   ! Empty!
end type t
end
