! { dg-options "-fno-openmp -fopenmp-simd" }

subroutine test1
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
end subroutine test1

subroutine test2
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test2

subroutine test3
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end do
end subroutine test3

subroutine test4
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
  !$omp end do
end subroutine test4

subroutine test5
  implicit none
  integer :: i

  !$omp unroll
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
end subroutine test5

subroutine test6
  implicit none
  integer :: i

  !$omp unroll
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test6

subroutine test7
  implicit none
  integer :: i

  !$omp unroll
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
  !$omp end unroll
end subroutine test7

subroutine test8
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
end subroutine test8

subroutine test9
  implicit none
  integer :: i

  !$omp unroll full
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
end subroutine test9

subroutine test10
  implicit none
  integer :: i,j

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
    do j = 1,100
      call dummy2(i,j)
    end do
  end do
end subroutine test10

subroutine test11
  implicit none
  integer :: i,j

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    !$omp unroll
    !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
    call dummy(i) ! { dg-error "Unexpected CALL statement at \\\(1\\\)" }
    !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
    do j = 1,100
      call dummy2(i,j)
    end do
  end do
end subroutine test11

subroutine test12
  implicit none
  integer :: i,j

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    !$omp unroll
    !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
    !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
    do j = 1,100
      call dummy2(i,j)
    end do
    call dummy(i)
  end do
end subroutine test12

subroutine test13
  implicit none
  integer :: i

  !$omp unroll
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
  !$omp end unroll
  !$omp end unroll ! { dg-error "Unexpected !\\\$OMP END UNROLL statement at \\\(1\\\)" }
end subroutine test13

subroutine test14
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  !$omp unroll ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
  !$omp end unroll
  !$omp end unroll ! { dg-error "Unexpected !\\\$OMP END UNROLL statement at \\\(1\\\)" }
end subroutine test14

subroutine test17
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll partial(0) ! { dg-error "PARTIAL clause argument not constant positive integer at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test17

subroutine test18
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll partial(-10) ! { dg-error "PARTIAL clause argument not constant positive integer at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test18
