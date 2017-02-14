! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine test
  integer, pointer :: p

  !$acc host_data use_device(p) ! { dg-warning "is used uninitialized in this function" }
  !$acc end host_data
end subroutine test

