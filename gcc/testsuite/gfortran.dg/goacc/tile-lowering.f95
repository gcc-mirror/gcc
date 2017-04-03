! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine par
  integer i, j, k

  !$acc parallel
  !$acc loop tile (1)
  do i = 1, 10
  end do

  !$acc loop tile (*)
  do i = 1, 10
  end do

  !$acc loop tile (1,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (*,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (1,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (*,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (1,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc loop tile (*,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc loop tile (1,*,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc loop tile (1,2,*)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do
  !$acc end parallel
end subroutine par

subroutine kerns
  integer i, j, k

  !$acc kernels
  !$acc loop tile (1)
  do i = 1, 10
  end do

  !$acc loop tile (*)
  do i = 1, 10
  end do

  !$acc loop tile (1,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (*,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (1,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (*,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc loop tile (1,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc loop tile (*,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc loop tile (1,*,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc loop tile (1,2,*)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do
  !$acc end kernels
end subroutine kerns

subroutine parloop
  integer i, j, k

  !$acc parallel loop tile (1)
  do i = 1, 10
  end do

  !$acc parallel loop tile (*)
  do i = 1, 10
  end do

  !$acc parallel loop tile (1,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc parallel loop tile (*,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc parallel loop tile (1,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc parallel loop tile (*,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc parallel loop tile (1,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc parallel loop tile (*,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc parallel loop tile (1,*,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc parallel loop tile (1,2,*)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do
end subroutine parloop

subroutine kernloop
  integer i, j, k

  !$acc kernels loop tile (1)
  do i = 1, 10
  end do

  !$acc kernels loop tile (*)
  do i = 1, 10
  end do

  !$acc kernels loop tile (1,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc kernels loop tile (*,2)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc kernels loop tile (1,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc kernels loop tile (*,*)
  do i = 1, 10
     do j = 1, 10
     end do
  end do

  !$acc kernels loop tile (1,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc kernels loop tile (*,2,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc kernels loop tile (1,*,3)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do

  !$acc kernels loop tile (1,2,*)
  do i = 1, 10
     do j = 1, 10
        do k = 1, 10
        end do
     end do
  end do
end subroutine kernloop


! { dg-final { scan-tree-dump-times "tile\\(1\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(0\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(1, 2\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(0, 2\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(1, 0\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(0, 0\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(1, 2, 3\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(0, 2, 3\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(1, 0, 3\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "tile\\(1, 2, 0\\)" 4 "original" } }
! { dg-final { scan-tree-dump-times "for \\(" 88 "original" } }
! { dg-final { scan-tree-dump-times "while \\(" 0 "original" } }
