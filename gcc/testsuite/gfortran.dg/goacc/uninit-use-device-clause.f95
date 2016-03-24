! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine test
  integer :: i

  !$acc host_data use_device(i) ! { dg-warning "is used uninitialized in this function" }
  !$acc end host_data
end subroutine test

