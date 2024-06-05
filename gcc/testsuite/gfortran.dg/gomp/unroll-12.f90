subroutine test1
  implicit none
  integer :: i
  !$omp unroll
  do while (i < 10) ! { dg-error "!\\\$OMP UNROLL cannot be a DO WHILE or DO without loop control at \\\(1\\\)" }
    call dummy(i)
    i = i + 1
  end do
end subroutine test1

subroutine test2
  implicit none
  integer :: i
  !$omp unroll
  do ! { dg-error "!\\\$OMP UNROLL cannot be a DO WHILE or DO without loop control at \\\(1\\\)" }
    call dummy(i)
    i = i + 1
    if (i >= 10) exit
  end do
end subroutine test2

subroutine test3
  implicit none
  integer :: i
  !$omp unroll
  do concurrent (i=1:10) ! { dg-error "!\\\$OMP UNROLL cannot be a DO CONCURRENT loop at \\\(1\\\)" }
    call dummy(i) ! { dg-error "Subroutine call to 'dummy' in DO CONCURRENT block at \\\(1\\\) is not PURE" }
  end do
end subroutine test3
