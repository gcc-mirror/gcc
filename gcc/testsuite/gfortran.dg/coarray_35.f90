! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! To be used with coarray_35a.f90
! Check that the coarray declared in the module is accessible
! by checking the assembler name
!
! Contributed by Alessandro Fanfarillo.
!
module global_coarrays
  implicit none
  integer,parameter :: n=10
  integer :: b(10)[*]
end module global_coarrays

! Check for the symbol of the coarray token (w/o system-dependend prefix)
! { dg-final { scan-assembler "caf_token__global_coarrays_MOD_b" } }
