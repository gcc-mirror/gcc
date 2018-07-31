! Test valid usages of the if_present clause.

! { dg-additional-options "-fdump-tree-omplower" }

subroutine t
  implicit none
  integer a, b, c(10)
  real, allocatable :: x, y, z(:)
  
  a = 5
  b = 10
  c(:) = -1

  allocate (x, y, z(100))
  
  !$acc update self(a) if_present
  !$acc update device(b) if_present async
  !$acc update host(c(1:3)) wait(4) if_present
  !$acc update self(c) device(a) host(b) if_present async(10) if(a == 10) 

  !$acc update self(x) if_present
  !$acc update device(y) if_present async
  !$acc update host(z(1:3)) wait(3) if_present
  !$acc update self(z) device(y) host(x) if_present async(4) if(a == 1) 
end subroutine t

! { dg-final { scan-tree-dump-times " if_present" 8 "omplower" } }
