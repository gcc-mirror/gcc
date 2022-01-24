! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine test
  integer, pointer :: p
  ! { dg-note {'p' was declared here} {} { target *-*-* } .-1 }

  !$acc host_data use_device(p) ! { dg-warning "is used uninitialized" }
  !$acc end host_data
end subroutine test

