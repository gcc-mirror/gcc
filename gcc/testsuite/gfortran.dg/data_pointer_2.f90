! { dg-do compile }
! { dg-options "-O -g" }
! PR fortran/77693 - ICE in rtl_for_decl_init
! Contributed by G.Steinmetz

program p
  implicit none
  complex, target  :: y    = (1.,2.)
  complex, target  :: z(2) = (3.,4.)
  complex, pointer :: a => y
  complex, pointer :: b => z(1)
  complex, pointer :: c, d, e
  data c /NULL()/   ! Valid
  data d /y/        ! Valid
  data e /(1.,2.)/  ! { dg-error "Pointer assignment target" }
  if (associated (a)) print *, a% re
  if (associated (b)) print *, b% im
  if (associated (c)) print *, c% re
  if (associated (d)) print *, d% im
  if (associated (e)) print *, e% re
end
