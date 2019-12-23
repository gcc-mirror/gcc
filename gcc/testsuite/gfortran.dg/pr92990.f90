! { dg-do compile }
! PR fortran/92990
! Verify fix of error message for NULLIFY vs. pointer assignment (PR70853)
program p
  integer, pointer :: x(:)
  type t
     integer, pointer :: y(:)
  end type t
  type(t) :: z
  nullify (x(1:2)) ! { dg-error "does not allow bounds remapping" }
  nullify (z%y(:)) ! { dg-error "does not allow bounds remapping" }
end
