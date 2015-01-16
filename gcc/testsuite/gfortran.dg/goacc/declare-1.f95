! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original" } 

program test
  implicit none
  integer :: i

  !$acc declare copy(i)
contains
  real function foo(n)
    integer, value :: n
    BLOCK
       integer i
       !$acc declare copy(i)
    END BLOCK
  end function foo
end program test
! { dg-prune-output "unimplemented" }
! { dg-final { scan-tree-dump-times "pragma acc declare map\\(force_tofrom:i\\)" 2 "original" } } 
! { dg-final { cleanup-tree-dump "original" } } 
