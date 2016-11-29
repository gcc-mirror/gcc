! { dg-do link }
! { dg-additional-sources pr77420_4.f90 }
!
module h5global
  implicit none
  integer :: h5p_default_f, h5p_flags
  equivalence(h5p_flags, h5p_default_f)
end module h5global
! { dg-final { cleanup-modules "h5global" } }
