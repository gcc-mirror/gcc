subroutine test1(i)
  implicit none
  integer :: i
  !$omp unroll ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  do i = 1,10
     call dummy(i)
  end do
end subroutine test1

subroutine test2(i)
  implicit none
  integer :: i
  !$omp unroll full ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  !$omp unroll
  do i = 1,10
     call dummy(i)
  end do
end subroutine test2

subroutine test3(i)
  implicit none
  integer :: i
  !$omp unroll full ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} }
  !$omp unroll ! { dg-warning {\!\$OMP UNROLL without PARTIAL clause at \(1\) turns loop into a non-loop} }
  !$omp unroll full
  !$omp unroll
  do i = 1,10
     call dummy(i)
  end do
end subroutine test3

subroutine test4(i)
  implicit none
  integer :: i
  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  do i = 1,10
     call dummy(i)
  end do
end subroutine test4

subroutine test5(i)
  implicit none
  integer :: i
  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  !$omp unroll
  do i = 1,10
     call dummy(i)
  end do
end subroutine test5

subroutine test6(i)
  implicit none
  integer :: i
  !$omp do ! { dg-error {missing canonical loop nest after \!\$OMP DO at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  !$omp unroll
  do i = 1,10
     call dummy(i)
  end do
end subroutine test6

subroutine test7(i)
  implicit none
  integer :: i
  !$omp loop ! { dg-error {missing canonical loop nest after \!\$OMP LOOP at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
  !$omp unroll
  do i = 1,10
     call dummy(i)
  end do
end subroutine test7
