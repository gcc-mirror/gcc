subroutine test1(i)
  implicit none
  integer :: i
  !$omp unroll
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test1

subroutine test2(i)
  implicit none
  integer :: i
  !$omp unroll full
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test2

subroutine test3(i)
  implicit none
  integer :: i
  !$omp unroll full
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test3

subroutine test4(i)
  implicit none
  integer :: i
  !$omp do
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test4

subroutine test5(i)
  implicit none
  integer :: i
  !$omp do
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test5

subroutine test6(i)
  implicit none
  integer :: i
  !$omp do
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test6

subroutine test7(i)
  implicit none
  integer :: i
  !$omp loop
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,10
    call dummy(i)
  end do
end subroutine test7
