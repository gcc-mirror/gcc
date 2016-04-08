! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine test
  INTEGER :: i

  !$acc parallel
  i = 1
  !$acc end parallel
end subroutine test

subroutine test2
  INTEGER :: i

  !$acc parallel firstprivate (i) ! { dg-warning "is used uninitialized in this function" }
  i = 1
  !$acc end parallel
end subroutine test2
