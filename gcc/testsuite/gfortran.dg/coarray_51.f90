! { dg-do link }
! { dg-options "-fcoarray=shared -fdump-tree-original" }
!
! Test that -fcoarray=shared is accepted and generates the same
! library-based CAF calls as -fcoarray=lib.

program test
  implicit none
  integer :: x[*]
  x = this_image ()
  sync all
end program test

! { dg-final { scan-tree-dump-times "_gfortran_caf_init \\(&argc, &argv\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_all \\(" 1 "original" } }
