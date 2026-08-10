! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/103474
!
! ICE in simplify_cobound on a coarray whose declaration was rejected.
!
! Contributed by G. Steinmetz  <gscfq@t-online.de>

program p
  type t
    integer :: a
  end type
  class(t) :: x[:]  ! { dg-error "shall not have codimensions with deferred shape" }
  print *, ucobound (x)
  if (any (lcobound (x) < 1)) stop
end
