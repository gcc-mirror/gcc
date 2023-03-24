subroutine test1
  implicit none
  integer :: i

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
end subroutine test1

subroutine test2
  implicit none
  integer :: i

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test2

subroutine test3
  implicit none
  integer :: i

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end do
end subroutine test3

subroutine test4
  implicit none
  integer :: i

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
  !$omp end do
end subroutine test4

subroutine test5
  implicit none
  integer :: i

  !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
end subroutine test5

subroutine test6
  implicit none
  integer :: i

  !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test6

subroutine test7
  implicit none
  integer :: i

  !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test7

subroutine test8
  implicit none
  integer :: i

  !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
  !$omp end unroll
end subroutine test8

subroutine test9
  implicit none
  integer :: i

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
end subroutine test9

subroutine test10
  implicit none
  integer :: i

  !$omp unroll full ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
end subroutine test10

subroutine test11
  implicit none
  integer :: i,j

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
     do j = 1,100
        call dummy2(i,j)
     end do
  end do
end subroutine test11

subroutine test12
  implicit none
  integer :: i,j

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
     call dummy(i) ! { dg-error {Unexpected CALL statement at \(1\)} }
  !$omp unroll
     do j = 1,100
        call dummy2(i,j)
     end do
  end do
end subroutine test12

subroutine test13
  implicit none
  integer :: i,j

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
     !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
     !$omp unroll
     do j = 1,100
        call dummy2(i,j)
     end do
     call dummy(i)
  end do
end subroutine test13

subroutine test14
  implicit none
  integer :: i

  !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
  !$omp end unroll
  !$omp end unroll ! { dg-error {Unexpected \!\$OMP END UNROLL statement at \(1\)} }
end subroutine test14

subroutine test15
  implicit none
  integer :: i

  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  !$omp unroll
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
  !$omp end unroll
  !$omp end unroll ! { dg-error {Unexpected \!\$OMP END UNROLL statement at \(1\)} }
end subroutine test15

subroutine test16
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial(1)
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test16

subroutine test17
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial(2)
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test17

subroutine test18
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial(0) ! { dg-error {PARTIAL clause argument not constant positive integer at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test18

subroutine test19
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial(-10) ! { dg-error {PARTIAL clause argument not constant positive integer at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test19

subroutine test20
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial
  do i = 1,100
     call dummy(i)
  end do
  !$omp end unroll
end subroutine test20

subroutine test21
  implicit none
  integer :: i

  !$omp unroll partial ! { dg-error {\!\$OMP UNROLL invalid around DO CONCURRENT loop at \(1\)} }
  do concurrent  (i = 1:100)
     call dummy(i) ! { dg-error {Subroutine call to 'dummy' in DO CONCURRENT block at \(1\) is not PURE} }
  end do
  !$omp end unroll
end subroutine test21

subroutine test22
  implicit none
  integer :: i

  !$omp do
  !$omp unroll partial
  do concurrent  (i = 1:100)  ! { dg-error {\!\$OMP DO cannot be a DO CONCURRENT loop at \(1\)} }
     call dummy(i) ! { dg-error {Subroutine call to 'dummy' in DO CONCURRENT block at \(1\) is not PURE} }
  end do
  !$omp end unroll
end subroutine test22
