subroutine test1
  implicit none
  integer :: i
  !$omp unroll ! { dg-error {\!\$OMP UNROLL invalid around DO WHILE or DO without loop control at \(1\)} }
  do while (i < 10)
     call dummy(i)
     i = i + 1
  end do
end subroutine test1

subroutine test2
  implicit none
  integer :: i
  !$omp unroll ! { dg-error {\!\$OMP UNROLL invalid around DO WHILE or DO without loop control at \(1\)} }
  do
     call dummy(i)
     i = i + 1
     if (i >= 10) exit
  end do
end subroutine test2

subroutine test3
  implicit none
  integer :: i
  !$omp unroll ! { dg-error {\!\$OMP UNROLL invalid around DO CONCURRENT loop at \(1\)} }
  do concurrent (i=1:10)
     call dummy(i) ! { dg-error {Subroutine call to 'dummy' in DO CONCURRENT block at \(1\) is not PURE} }
  end do
end subroutine test3
