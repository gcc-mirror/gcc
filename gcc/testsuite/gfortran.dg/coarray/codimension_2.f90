! { dg-do link }
! { dg-additional-sources "codimension_2a.f90 codimension_2b.f90" }
!
! To be used with codimension_2a.f90
! Check that the coarray declared in the module is accessible
! by doing a link test
!
! Contributed by Alessandro Fanfarillo.
!
module global_coarrays
  implicit none
  integer,parameter :: n=10
  integer :: b(10)[*]
end module global_coarrays
