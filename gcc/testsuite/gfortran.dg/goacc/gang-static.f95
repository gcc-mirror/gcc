! { dg-do compile }
! { dg-additional-options "-fdump-tree-omplower" }

program main
  integer, parameter :: n = 100
  integer i, a(n), b(n)

  do i = 1, n
     b(i) = i
  end do

  !$acc parallel loop gang (static:*) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 0
  end do
  !$acc end parallel loop

  call test (a, b, 0, n)

  !$acc parallel loop gang (static:1) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 1
  end do
  !$acc end parallel loop

  call test (a, b, 1, n)

  !$acc parallel loop gang (static:2) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 2
  end do
  !$acc end parallel loop

  call test (a, b, 2, n)

  !$acc parallel loop gang (static:5) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 5
  end do
  !$acc end parallel loop

  call test (a, b, 5, n)

  !$acc parallel loop gang (static:20) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 20
  end do
  !$acc end parallel loop

  !$acc kernels loop gang (num:5, static:*)
  do i = 1, n
     a(i) = b(i) + 20
  end do
  !$acc end kernels loop

  !$acc kernels loop gang (static:20, num:30)
  do i = 1, n
     a(i) = b(i) + 20
  end do
  !$acc end kernels loop

  call test (a, b, 20, n)

end program main

subroutine test (a, b, sarg, n)
  integer n
  integer a (n), b(n), sarg
  integer i

  do i = 1, n
     if (a(i) .ne. b(i) + sarg) call abort ()
  end do
end subroutine test

! { dg-final { scan-tree-dump-times "gang\\(static:\\\*\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "gang\\(static:1\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "gang\\(static:2\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "gang\\(static:5\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "gang\\(static:20\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "gang\\(num: 5 static:\\\*\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "gang\\(num: 30 static:20\\)" 1 "omplower" } }
