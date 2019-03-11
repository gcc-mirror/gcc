! { dg-do compile }
!
! PR fortran/88326 - ICE in gfc_conv_array_initializer

program p
  character, parameter :: x(3) = ['a','b','c']
  character    :: y(1) = transfer('', x) ! { dg-error "Different shape for array assignment" }
  character(0) :: z(1) = transfer('', x) ! { dg-error "Different shape for array assignment" }
  character    :: u(0) = transfer('', x)
  print *, y, z, u
end
