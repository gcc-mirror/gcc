! Explicit 'private' clauses related to 'do' loops inside an OpenACC
! 'serial' construct.

! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

! (The 'independent' clauses are used as end of directive markers in tree dump
! scanning.)

program test
  implicit none
  integer :: i0_1
  integer :: i0_2, j0_2
  integer :: i1_s
  integer :: i1_c
  integer :: i2_1_s, j2_1_s
  integer :: i2_1_c, j2_1_c
  integer :: i2_2_s, j2_2_s
  integer :: i2_3_s, j2_3_s
  integer :: i2_3_c, j2_3_c
  integer :: i3_1_s, j3_1_s, k3_1_s
  integer :: i3_1_c, j3_1_c, k3_1_c
  integer :: i3_2_s, j3_2_s, k3_2_s
  integer :: i3_2_c, j3_2_c, k3_2_c
  integer :: i3_3_s, j3_3_s, k3_3_s
  integer :: i3_3_c, j3_3_c, k3_3_c
  integer :: i3_4_s, j3_4_s, k3_4_s
  integer :: i3_4_c, j3_4_c, k3_4_c
  integer :: i3_5_s, j3_5_s, k3_5_s

  !$acc serial private(i0_1)
  ! { dg-final { scan-tree-dump-times "#pragma acc serial private\\(i0_1\\)" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma omp target oacc_serial private\\(i0_1\\)" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "firstprivate\\(i0_1\\)" 0 "gimple" } }
  do i0_1 = 1, 100
  end do
  !$acc end serial

  !$acc serial private(i0_2, j0_2)
  ! { dg-final { scan-tree-dump-times "#pragma acc serial private\\(i0_2\\) private\\(j0_2\\)" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma omp target oacc_serial private\\(i0_2\\) private\\(j0_2\\)" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "firstprivate\\(i0_2\\)" 0 "gimple" } }
  ! { dg-final { scan-tree-dump-times "firstprivate\\(j0_2\\)" 0 "gimple" } }
  do i0_2 = 1, 100
     do j0_2 = 1, 100
     end do
  end do
  !$acc end serial

  !$acc serial
  !$acc loop private(i1_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i1_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i1_s\\) independent" 1 "gimple" } }
  do i1_s = 1, 100
  end do
  !$acc end serial

  !$acc serial loop private(i1_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i1_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i1_c\\) independent" 1 "gimple" } }
  do i1_c = 1, 100
  end do

  !$acc serial
  !$acc loop private(i2_1_s, j2_1_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_1_s\\) private\\(j2_1_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_1_s\\) private\\(j2_1_s\\) independent" 1 "gimple" } }
  do i2_1_s = 1, 100
     do j2_1_s = 1, 100
     end do
  end do
  !$acc end serial

  !$acc serial loop private(i2_1_c, j2_1_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_1_c\\) private\\(j2_1_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_1_c\\) private\\(j2_1_c\\) independent" 1 "gimple" } }
  do i2_1_c = 1, 100
     do j2_1_c = 1, 100
     end do
  end do

  !$acc serial private(i2_2_s)
  ! { dg-final { scan-tree-dump-times "#pragma acc serial private\\(i2_2_s\\)" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma omp target oacc_serial private\\(i2_2_s\\)" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "firstprivate\\(i2_2_s\\)" 0 "gimple" } }
  do i2_2_s = 1, 100
     !$acc loop private(j2_2_s) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j2_2_s\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j2_2_s\\) independent" 1 "gimple" } }
     do j2_2_s = 1, 100
     end do
  end do
  !$acc end serial

  !$acc serial
  !$acc loop private(i2_3_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_3_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_3_s\\) independent" 1 "gimple" } }
  do i2_3_s = 1, 100
     !$acc loop private(j2_3_s) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j2_3_s\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j2_3_s\\) independent" 1 "gimple" } }
     do j2_3_s = 1, 100
     end do
  end do
  !$acc end serial

  !$acc serial loop private(i2_3_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_3_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i2_3_c\\) independent" 1 "gimple" } }
  do i2_3_c = 1, 100
     !$acc loop private(j2_3_c) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j2_3_c\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j2_3_c\\) independent" 1 "gimple" } }
     do j2_3_c = 1, 100
     end do
  end do

  !$acc serial
  !$acc loop private(i3_1_s, j3_1_s, k3_1_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_1_s\\) private\\(j3_1_s\\) private\\(k3_1_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_1_s\\) private\\(j3_1_s\\) private\\(k3_1_s\\) independent" 1 "gimple" } }
  do i3_1_s = 1, 100
     do j3_1_s = 1, 100
        do k3_1_s = 1, 100
        end do
     end do
  end do
  !$acc end serial

  !$acc serial loop private(i3_1_c, j3_1_c, k3_1_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_1_c\\) private\\(j3_1_c\\) private\\(k3_1_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_1_c\\) private\\(j3_1_c\\) private\\(k3_1_c\\) independent" 1 "gimple" } }
  do i3_1_c = 1, 100
     do j3_1_c = 1, 100
        do k3_1_c = 1, 100
        end do
     end do
  end do

  !$acc serial
  !$acc loop private(i3_2_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_2_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_2_s\\) independent" 1 "gimple" } }
  do i3_2_s = 1, 100
     !$acc loop private(j3_2_s, k3_2_s) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_2_s\\) private\\(k3_2_s\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_2_s\\) private\\(k3_2_s\\) independent" 1 "gimple" } }
     do j3_2_s = 1, 100
        do k3_2_s = 1, 100
        end do
     end do
  end do
  !$acc end serial

  !$acc serial loop private(i3_2_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_2_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_2_c\\) independent" 1 "gimple" } }
  do i3_2_c = 1, 100
     !$acc loop private(j3_2_c, k3_2_c) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_2_c\\) private\\(k3_2_c\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_2_c\\) private\\(k3_2_c\\) independent" 1 "gimple" } }
     do j3_2_c = 1, 100
        do k3_2_c = 1, 100
        end do
     end do
  end do

  !$acc serial
  !$acc loop private(i3_3_s, j3_3_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_3_s\\) private\\(j3_3_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_3_s\\) private\\(j3_3_s\\) independent" 1 "gimple" } }
  do i3_3_s = 1, 100
     do j3_3_s = 1, 100
        !$acc loop private(k3_3_s) independent
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_3_s\\) independent" 1 "original" } }
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_3_s\\) independent" 1 "gimple" } }
        do k3_3_s = 1, 100
        end do
     end do
  end do
  !$acc end serial

  !$acc serial loop private(i3_3_c, j3_3_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_3_c\\) private\\(j3_3_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_3_c\\) private\\(j3_3_c\\) independent" 1 "gimple" } }
  do i3_3_c = 1, 100
     do j3_3_c = 1, 100
        !$acc loop private(k3_3_c) independent
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_3_c\\) independent" 1 "original" } }
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_3_c\\) independent" 1 "gimple" } }
        do k3_3_c = 1, 100
        end do
     end do
  end do

  !$acc serial
  !$acc loop private(i3_4_s) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_4_s\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_4_s\\) independent" 1 "gimple" } }
  do i3_4_s = 1, 100
     !$acc loop private(j3_4_s) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_4_s\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_4_s\\) independent" 1 "gimple" } }
     do j3_4_s = 1, 100
        !$acc loop private(k3_4_s) independent
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_4_s\\) independent" 1 "original" } }
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_4_s\\) independent" 1 "gimple" } }
        do k3_4_s = 1, 100
        end do
     end do
  end do
  !$acc end serial

  !$acc serial loop private(i3_4_c) independent
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_4_c\\) independent" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(i3_4_c\\) independent" 1 "gimple" } }
  do i3_4_c = 1, 100
     !$acc loop private(j3_4_c) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_4_c\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_4_c\\) independent" 1 "gimple" } }
     do j3_4_c = 1, 100
        !$acc loop private(k3_4_c) independent
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_4_c\\) independent" 1 "original" } }
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_4_c\\) independent" 1 "gimple" } }
        do k3_4_c = 1, 100
        end do
     end do
  end do

  !$acc serial private(i3_5_s)
  ! { dg-final { scan-tree-dump-times "#pragma acc serial private\\(i3_5_s\\)" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "#pragma omp target oacc_serial private\\(i3_5_s\\)" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "firstprivate\\(i3_5_s\\)" 0 "gimple" } }
  do i3_5_s = 1, 100
     !$acc loop private(j3_5_s) independent
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_5_s\\) independent" 1 "original" } }
     ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(j3_5_s\\) independent" 1 "gimple" } }
     do j3_5_s = 1, 100
        !$acc loop private(k3_5_s) independent
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_5_s\\) independent" 1 "original" } }
        ! { dg-final { scan-tree-dump-times "#pragma acc loop private\\(k3_5_s\\) independent" 1 "gimple" } }
        do k3_5_s = 1, 100
        end do
     end do
  end do
  !$acc end serial
end program test
