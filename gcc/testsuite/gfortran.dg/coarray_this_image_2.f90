! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
j1 = this_image(distance=4)
j2 = this_image(5)
k1 = num_images()
k2 = num_images(6)
k3 = num_images(distance=7)
k4 = num_images(distance=8, failed=.true.)
k5 = num_images(failed=.false.)
end

! { dg-final { scan-tree-dump-times "j1 = _gfortran_caf_this_image \\(4\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "j2 = _gfortran_caf_this_image \\(5\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "k1 = _gfortran_caf_num_images \\(0, -1\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "k2 = _gfortran_caf_num_images \\(6, -1\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "k3 = _gfortran_caf_num_images \\(7, -1\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "k4 = _gfortran_caf_num_images \\(8, 1\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "k5 = _gfortran_caf_num_images \\(0, 0\\);" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
