! { dg-do  run }
! { dg-options "-O -finline-matmul-limit=100 -fdump-tree-optimized" }
! PR 80975 - this did not zero the result array in the library version;
! make sure this also doesn't happen in the inline version.
program bogus_matmul
  implicit none
  real :: M(3,0), v(0), w(3)

  w = 7
  w = matmul(M,v)
  if( any(w .ne. 0) ) then
    STOP 1
  end if
end program bogus_matmul
! { dg-final { scan-tree-dump-times "matmul_r4" 0 "optimized" } }

