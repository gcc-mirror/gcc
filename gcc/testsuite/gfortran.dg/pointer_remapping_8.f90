! { dg-do compile }
!
! PR 54788 ICE on pointer-array element assignment
!
program bug
  integer, pointer :: a(:)
  integer :: b
  allocate(a(0:0))
  a(0:0) => b ! { dg-error "Rank remapping target must be rank 1 or simply contiguous" }
end
