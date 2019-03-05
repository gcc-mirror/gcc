! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR88579 - Test optimizations for bases that are powers of 2 or -2.
program p
  implicit none
  integer(4) :: i, u
  integer(1) :: j, v
  integer(2) :: k, w
  integer(8) :: z
  ! Test selected positive bases
  u = 1
  do i=1,5
     u = u * 64_4
     if (u /= 64_4 ** i) stop 1
  end do
  z = 1
  do i=1,7
     z = z * 256_8
     if (z /= 256_8 ** i) stop 2
  end do
  z = 1
  do i=1,3
     z = z * 65536_8
     if (z /= 65536_8 ** i) stop 3
  end do
  ! Test selected negative bases and integer kind combinations
  u = 1
  do i=1,7
     u = u * (-2_1)
     if (u /= (-2_1) ** i) stop 4
  end do
  v = 1
  do j=1,7
     v = v * (-2_1)
     if (v /= (-2_1) ** j) stop 5
  end do
  v = 1
  do k=1,7
     v = v * (-2_1)
     if (v /= (-2_1) ** k) stop 6
  end do
  w = 1
  do k=1,7
     w = w * (-4_2)
     if (w /= (-4_2) ** k) stop 7
  end do
  w = 1
  do i=1,5
     w = w * (-8_2)
     if (w /= (-8_2) ** i) stop 8
  end do
  u = 1
  do i=1,1
     u = u * (-HUGE(1_4)/2-1)
     if (u /= (-HUGE(1_4)/2-1) ** i) stop 9
  end do
  z = 1
  do i=1,7
     z = z * (-512_8)
     if (z /= (-512_8) ** i) stop 10
  end do
end program p
! { dg-final { scan-tree-dump-not "_gfortran_pow" "original" } }
