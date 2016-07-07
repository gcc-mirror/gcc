! { dg-do compile }
! { dg-options "-O2 -fdump-tree-profile_estimate" }

subroutine test(block, array)
integer :: i,j, block(9), array(2)

do i = array(1), array(2), 2
    do j = array(1), array(2), 3
       block(i) = j
    end do
end do
end subroutine test

! { dg-final { scan-tree-dump-times "Fortran loop preheader heuristics of edge" 2 "profile_estimate" } }
! { dg-final { scan-tree-dump-times "loop guard" 0 "profile_estimate" } }
