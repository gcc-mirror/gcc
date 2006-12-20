! { dg-do run }
! This tests the fix for PR30190, in which the array reference
! in the associated statement would cause a segfault.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
  TYPE particle_type
     INTEGER, POINTER :: p(:)
  END TYPE particle_type
  TYPE(particle_type), POINTER  :: t(:)
  integer :: i
  logical :: f
  i = 1
  allocate(t(1))
  allocate(t(1)%p(0))
  f = associated(t(i)%p,t(i)%p)
end
