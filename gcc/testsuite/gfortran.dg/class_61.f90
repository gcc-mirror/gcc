! { dg-do compile }
!
! PR 78573: [7 Regression] [OOP] ICE in resolve_component, at fortran/resolve.c:13405
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

program p
  type t1
    class(t2), pointer :: q(2)  ! { dg-error "must have a deferred shape" }
  end type
end
