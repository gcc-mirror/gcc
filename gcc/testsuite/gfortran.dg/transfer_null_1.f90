! { dg-do compile }
! Test fix for pr38763, where NULL was not being encoded.
!
! Contributed by Steve Kargl <kargl@gcc.gnu.org> from a
! posting by James van Buskirk on clf.
!
program sizetest
   use ISO_C_BINDING
   implicit none
   integer, parameter :: ik1 = selected_int_kind(2)
   TYPE vehicle_t1
      INTEGER(C_INT), DIMENSION(:), ALLOCATABLE :: sensors
   END TYPE vehicle_t1
   type(vehicle_t1) gfortran_bug_workaround
   integer i
   i = size(transfer(vehicle_t1(NULL()),[0_ik1]))
   print *, i
   i = size(transfer(vehicle_t1([i]),[0_ik1]))
   print *, i
end program sizetest
