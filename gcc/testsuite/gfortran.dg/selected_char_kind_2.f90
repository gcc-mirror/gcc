! { dg-do compile }
!
! Check that nonexisting character kinds are not rejected by the compiler
!
  character(kind=selected_char_kind("")) :: s1 ! { dg-error "is not supported for CHARACTER" }
  character(kind=selected_char_kind("     ")) :: s2 ! { dg-error "is not supported for CHARACTER" }
  character(kind=selected_char_kind("asciii")) :: s3 ! { dg-error "is not supported for CHARACTER" }
  character(kind=selected_char_kind("I don't exist")) :: s4 ! { dg-error "is not supported for CHARACTER" }

  print *, selected_char_kind() ! { dg-error "Missing actual argument" }
  print *, selected_char_kind(12) ! { dg-error "must be CHARACTER" }
  print *, selected_char_kind(["foo", "bar"]) ! { dg-error "must be a scalar" }

end
