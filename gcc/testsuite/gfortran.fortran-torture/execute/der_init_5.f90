! Check that null initialization of pointer components works.
! PR 15969 prompted these
! the commented out tests are cases where we still fail
program der_init_5
  type t
     type(t), pointer :: a => NULL()
     real, pointer :: b => NULL()
     character, pointer :: c => NULL()
     integer, pointer, dimension(:) :: d => NULL()
  end type t
  type (t) :: p
  if (associated(p%a)) STOP 1
  if (associated(p%b)) STOP 2
!  if (associated(p%c)) STOP 3
  if (associated(p%d)) STOP 4
end
