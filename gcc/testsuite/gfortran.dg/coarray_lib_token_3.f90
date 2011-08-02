! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
! Test coarray registering
!
integer, allocatable :: CAF(:)[:], caf_scalar[:]
allocate(CAF(1)[*])
allocate(CAF_SCALAR[*])
end

! { dg-final { scan-tree-dump-times "caf.data = \\(void . restrict\\) _gfortran_caf_register \\(4, 1, &caf.token, 0B, 0B, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "caf_scalar.data = \\(void . restrict\\) _gfortran_caf_register \\(4, 1, &caf_scalar.token, 0B, 0B, 0\\);" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
