! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! Test that the internal pack and unpack routines work OK
! for our large real type.

program main
  implicit none
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(kind=k), dimension(3) :: rk
  complex(kind=k), dimension(3) :: ck

  rk = (/ -1.0_k, 1.0_k, -3.0_k /)
  call sub_rk(rk(1:3:2))
  if (any(rk /= (/ 3.0_k, 1.0_k, 2.0_k/))) call abort

  ck = (/ (-1.0_k, 0._k), (1.0_k, 0._k), (-3.0_k, 0._k) /)
  call sub_ck(ck(1:3:2))
  if (any(real(ck) /= (/ 3.0_k, 1.0_k, 2.0_k/))) call abort
  if (any(aimag(ck) /= 0._k)) call abort

end program main

subroutine sub_rk(r)
  implicit none
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(kind=k), dimension(2) :: r
  if (r(1) /= -1._k) call abort
  if (r(2) /= -3._k) call abort
  r(1) = 3._k
  r(2) = 2._k
end subroutine sub_rk

subroutine sub_ck(r)
  implicit none
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  complex(kind=k), dimension(2) :: r
  if (r(1) /= (-1._k,0._k)) call abort
  if (r(2) /= (-3._k,0._k)) call abort
  r(1) = 3._k
  r(2) = 2._k
end subroutine sub_ck
