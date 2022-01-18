! { dg-do compile }
! { dg-prune-output "Obsolescent feature: Alternate-return argument" }
! PR fortran/99256 - ICE in variable_check
! Contributed by G.Steimetz

program test
  use iso_c_binding
  type(c_ptr)    :: i
  type(c_funptr) :: p
  call move_alloc     (*1, *2) ! { dg-error "ALTERNATE RETURN" }
  call c_f_pointer     (i, *1) ! { dg-error "ALTERNATE RETURN" }
  call c_f_procpointer (p, *2) ! { dg-error "ALTERNATE RETURN" }
1 continue
2 stop
end
