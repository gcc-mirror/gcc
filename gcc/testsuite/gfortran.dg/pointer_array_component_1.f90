! { dg-do run }
!
! Check the fix for PR34640 comment 28.
!
! This involves pointer array components that point to components of arrays
! of derived types.
!
  type var_tables
     real, pointer :: rvar(:)
  end type

  type real_vars
     real r
     real :: index
  end type

  type(var_tables) ::  vtab_r
  type(real_vars),  target :: x(2)
  real, pointer :: z(:)
  real :: y(2)

  x = [real_vars (11.0, 1.0), real_vars (42.0, 2.0)]
  vtab_r%rvar => x%r
  if (any (abs (vtab_r%rvar - [11.0, 42.0]) > 1.0e-5)) call abort  ! Check skipping 'index; is OK.

  y = vtab_r%rvar
  if (any (abs (y - [11.0, 42.0]) > 1.0e-5)) call abort  ! Check that the component is usable in assignment.

  call foobar (vtab_r, [11.0, 42.0])

  vtab_r = barfoo ()

  call foobar (vtab_r, [111.0, 142.0])

contains
  subroutine foobar (vtab, array)
    type(var_tables) ::  vtab
    real :: array (:)
    if (any (abs (vtab%rvar - array) > 1.0e-5)) call abort  ! Check passing as a dummy.
    if (abs (vtab%rvar(2) - array(2)) > 1.0e-5) call abort  ! Check component reference.
  end subroutine

  function barfoo () result(res)
    type(var_tables) ::  res
    allocate (res%rvar(2), source = [111.0, 142.0])  ! Check allocation
  end function
end
