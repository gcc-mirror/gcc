! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
! Check that allocating a coarray adds an implicit sync all.
 
 implicit none
 integer, allocatable :: f(:)[:]
 allocate( f(20)[*], source = 1 )
end

! { dg-final { scan-tree-dump-times "_gfortran_caf_sync_all \\(" 1 "original" } }
