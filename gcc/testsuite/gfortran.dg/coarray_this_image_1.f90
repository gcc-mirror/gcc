! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=single" }
!
j1 = this_image(distance=4)
j2 = this_image(5)
k1 = num_images()
k2 = num_images(6)
k3 = num_images(distance=7)
k4 = num_images(distance=8, failed=.true.)
k5 = num_images(failed=.false.)
end

! { dg-final { scan-tree-dump-times "j1 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "j2 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "k1 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "k2 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "k3 = 1;" 1 "original" } }
! { dg-final { scan-tree-dump-times "k4 = 0;" 1 "original" } }
! { dg-final { scan-tree-dump-times "k5 = 1;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
