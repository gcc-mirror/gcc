! { dg-do compile }
! PR fortran/86470 - ICE with OpenMP, class(*)

program p
  implicit none
  class(*), allocatable :: val
!$OMP PARALLEL private(val)
  allocate(integer::val)
  val = 1
  deallocate(val)
!$OMP END PARALLEL
end
