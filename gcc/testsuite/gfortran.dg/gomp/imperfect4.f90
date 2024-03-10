! This test case is expected to fail due to errors.

subroutine f1 (depth, iter)
  integer :: depth, iter
end subroutine

subroutine f2 (depth, iter)
  integer :: depth, iter
end subroutine

! Unlike the C/C++ front ends, the Fortran front end already has the whole
! parse tree for the OMP DO construct before doing error checking on it.
! It gives up immediately if there are not enough nested loops for the
! specified COLLAPSE depth, without error-checking intervening code.

subroutine s1 (a1, a2, a3)
  integer :: a1, a2, a3
  integer :: i, j, k

  !$omp do collapse(4)     ! { dg-error "not enough DO loops" }
  do i = 1, a1
    call f1 (1, i)
    do j = 1, a2
      call f1 (2, j)
      do k = 1, a3
! This is not valid intervening code, but the above error takes precedence.
!$omp barrier
        call f1 (3, k)
        call f2 (3, k)
      end do
      call f2 (2, j)
    end do
    call f2 (1, i)
  end do

end subroutine
