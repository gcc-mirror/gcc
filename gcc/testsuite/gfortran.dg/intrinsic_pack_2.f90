! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! Take the pack intrinsic through its paces, with all types that are
! normally accessible.
program main
  implicit none
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  integer :: i
  real(kind=k), dimension(3,3) :: rk
  real(kind=k), dimension(9) :: vrk
  real(kind=k), dimension(9) :: rrk
  complex(kind=k), dimension(3,3) :: ck
  complex(kind=k), dimension(9) :: vck
  complex(kind=k), dimension(9) :: rck

  vrk = (/(i+10,i=1,9)/)
  rk = reshape((/1.0_k, -3.0_k, 2.1_k, -4.21_k, 1.2_k, 0.98_k, -1.2_k, &
  &              -7.1_k, -9.9_k, 0.3_k /), shape(rk))
  rrk = pack(rk,rk>0,vrk)
  if (any(rrk /= (/ 1.0_k, 2.1_k, 1.2_k, 0.98_k,  15._k, 16._k, 17._k, &
  &                  18._k, 19._k /))) STOP 1

  vck = (/(i+10,i=1,9)/)
  ck = reshape((/1.0_k, -3.0_k, 2.1_k, -4.21_k, 1.2_k, 0.98_k, -1.2_k, &
  &              -7.1_k, -9.9_k, 0.3_k /), shape(ck))
  rck = pack(ck,real(ck)>0,vck)
  if (any(real(rck) /= (/ 1.0_k, 2.1_k, 1.2_k, 0.98_k,  15._k, 16._k, 17._k, &
  &                  18._k, 19._k /))) STOP 2
  if (any(aimag(rck) /= 0)) STOP 3

end program main
