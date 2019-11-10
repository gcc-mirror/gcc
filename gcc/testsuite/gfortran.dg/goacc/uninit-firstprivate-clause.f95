! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

subroutine test_parallel
  INTEGER :: i

  !$acc parallel
  i = 1
  !$acc end parallel
end subroutine test_parallel

subroutine test_serial
  INTEGER :: i

  !$acc serial
  i = 1
  !$acc end serial
end subroutine test_serial


subroutine test2_parallel
  INTEGER :: i
  ! { dg-note {'i' was declared here} {} { target *-*-* } .-1 }

  !$acc parallel firstprivate (i) ! { dg-warning "is used uninitialized" }
  i = 1
  !$acc end parallel
end subroutine test2_parallel

subroutine test2_serial
  INTEGER :: i
  ! { dg-note {'i' was declared here} {} { target *-*-* } .-1 }

  !$acc serial firstprivate (i) ! { dg-warning "is used uninitialized" }
  i = 1
  !$acc end serial
end subroutine test2_serial
