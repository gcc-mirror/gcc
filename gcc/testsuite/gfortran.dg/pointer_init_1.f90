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
  if (associated(a)) call abort()
  if (associated(b)) call abort()
  if (associated(c)) call abort()
  if (associated(d)) call abort()
end
