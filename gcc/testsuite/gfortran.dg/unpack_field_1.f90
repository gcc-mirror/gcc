! { dg-do compile }
! PR fortran/107922 - ICE in gfc_simplify_unpack
! Test error recovery when shapes of FIELD and MASK do not match
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(2) = 1
  integer, parameter :: d(3) = 1
  logical, parameter :: mask(3) = [.false.,.true.,.false.]
  integer, parameter :: b(2) = unpack(a,mask,a)          ! { dg-error "must have identical shape" }
  integer :: c(3) = unpack(a,[.false.,.true.,.false.],a) ! { dg-error "must have identical shape" }
  print *, unpack(a,mask,a)                              ! { dg-error "must have identical shape" }
  print *, unpack(a,mask,d) ! OK
  print *, unpack(a,mask,3) ! OK
end

! { dg-error "Cannot simplify expression" " " { target *-*-* } 12 }
