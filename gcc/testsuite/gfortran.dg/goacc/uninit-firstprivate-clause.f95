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
  ! { dg-note {'i' was declared here} {} { target *-*-* } .-1 }

  !$acc parallel firstprivate (i) ! { dg-warning "is used uninitialized" }
  i = 1
  !$acc end parallel
end subroutine test2
