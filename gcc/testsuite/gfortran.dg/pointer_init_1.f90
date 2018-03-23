! Check that null initialization of pointer variable works.
! { dg-do run }
program pointer_init_1
  type t
    real x
  end type
  type(t), pointer :: a => NULL()
  real, pointer :: b => NULL()
  character, pointer :: c => NULL()
  integer, pointer, dimension(:) :: d => NULL()
  if (associated(a)) STOP 1
  if (associated(b)) STOP 2
  if (associated(c)) STOP 3
  if (associated(d)) STOP 4
end
