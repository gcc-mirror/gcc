! { dg-do compile }
! PR fortran/104314 - ICE in deferred_op_assign
! Contributed by G.Steinmetz

program p
  character(:), allocatable :: c(:)
  c = ['123']
  c = c == c  ! { dg-error "Cannot convert" }
end
