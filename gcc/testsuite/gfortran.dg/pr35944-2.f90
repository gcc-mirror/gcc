! { dg-do run }
! { dg-require-effective-target fortran_large_real }

  implicit none
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  integer :: i
  real(k) :: qda1(10), qda(10), qval

  qda = (/ 1,2,3,4,5,6,7,8,9,10 /)
  QDA1 = MOD (1.1_k*(QDA(1)-5.0_k), P=(QDA-2.5_k))
  DO i = 1, 10
    QVAL = MOD (1.1_k*(QDA(1)-5.0_k), P=(QDA(i)-2.5_k))
    if (qval /= qda1(i)) call abort
  enddo
end
