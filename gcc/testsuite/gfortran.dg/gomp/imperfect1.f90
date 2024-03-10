! This test case is expected to fail due to errors.

subroutine f1 (depth, iter)
  integer :: depth, iter
end subroutine

subroutine f2 (depth, iter)
  integer :: depth, iter
end subroutine

subroutine s1 (a1, a2, a3)
  integer :: a1, a2, a3
  integer :: i, j, k

  !$omp do collapse(3) 
  do i = 1, a1
    call f1 (1, i)
    do j = 1, a2
      call f1 (2, j)
      if (i == 3) then
        cycle  ! { dg-error "CYCLE statement" }
      else
        exit   ! { dg-error "EXIT statement" }
      endif
!$omp barrier  ! { dg-error "OpenMP directive in intervening code" }
      do k = 1, a3
        call f1 (3, k)
        call f2 (3, k)
      end do
      call f2 (2, j)
    end do
    do k = 1, a3  ! { dg-error "loop in intervening code" }
      call f1 (3, k)
      call f2 (3, k)
    end do
    call f2 (1, i)
  end do

end subroutine
