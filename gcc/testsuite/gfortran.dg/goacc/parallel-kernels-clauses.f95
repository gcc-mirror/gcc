! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

! test clauses added in OpenACC ver 2.0

program test
  implicit none
  integer :: i, a(10), b(5:7)
  integer, parameter :: acc_async_noval = -1 
  integer, parameter :: acc_async_sync = -2
  logical :: l

  ! async
  !$acc kernels async(i)
  !$acc end kernels
  !$acc parallel async(i)
  !$acc end parallel

  !$acc kernels async(0, 1) { dg-error "Failed to match clause" }
  !$acc parallel async(0, 1) { dg-error "Failed to match clause" }

  !$acc kernels async
  !$acc end kernels
  !$acc parallel async
  !$acc end parallel

  !$acc kernels async(acc_async_noval)
  !$acc end kernels
  !$acc parallel async(acc_async_noval)
  !$acc end parallel

  !$acc kernels async(acc_async_sync)
  !$acc end kernels
  !$acc parallel async(acc_async_sync)
  !$acc end parallel

  !$acc kernels async() { dg-error "Invalid character" }
  !$acc parallel async() { dg-error "Invalid character" }

  !$acc kernels async("a") { dg-error "Failed to match clause" }
  !$acc parallel async("a") { dg-error "Failed to match clause" }

  !$acc kernels async(.true.) { dg-error "Failed to match clause" }
  !$acc parallel async(.true.) { dg-error "Failed to match clause" }

  ! default(none)
  !$acc kernels default(none)
  !$acc end kernels
  !$acc parallel default(none)
  !$acc end parallel

  !$acc kernels default (none)
  !$acc end kernels
  !$acc parallel default (none)
  !$acc end parallel

  !$acc kernels default ( none )
  !$acc end kernels
  !$acc parallel default ( none )
  !$acc end parallel

  !$acc kernels default { dg-error "Expected '\\(' after 'default'" }
  !$acc parallel default { dg-error "Expected '\\(' after 'default'" }

  !$acc kernels default() { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
  !$acc parallel default() { dg-error "Expected NONE or PRESENT in DEFAULT clause" }

  !$acc kernels default(i) { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
  !$acc parallel default(i) { dg-error "Expected NONE or PRESENT in DEFAULT clause" }

  !$acc kernels default(1) { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
  !$acc parallel default(1) { dg-error "Expected NONE or PRESENT in DEFAULT clause" }

  ! Wait
  !$acc kernels wait (l) ! { dg-error "INTEGER" }
  !$acc end kernels
  !$acc kernels wait (.true.) ! { dg-error "INTEGER" }
  !$acc end kernels
  !$acc kernels wait (i, 1) 
  !$acc end kernels
  !$acc kernels wait (a) ! { dg-error "INTEGER" }
  !$acc end kernels
  !$acc kernels wait (b(5:6)) ! { dg-error "INTEGER" }
  !$acc end kernels

  !$acc parallel wait (l) ! { dg-error "INTEGER" }
  !$acc end parallel
  !$acc parallel wait (.true.) ! { dg-error "INTEGER" }
  !$acc end parallel
  !$acc parallel wait (i, 1) 
  !$acc end parallel
  !$acc parallel wait (a) ! { dg-error "INTEGER" }
  !$acc end parallel
  !$acc parallel wait (b(5:6)) ! { dg-error "INTEGER" }
  !$acc end parallel
end
