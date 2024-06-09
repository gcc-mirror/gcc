! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Fix the regression caused by r14-9752 (fix for PR112407)
! Contributed by Orion Poplawski  <orion@nwra.com>
! Problem isolated by Jakub Jelinek  <jakub@gcc.gnu.org> and further
! reduced here.
!
module m
  type :: smoother_type
    integer :: i
  end type
  type :: onelev_type
    class(smoother_type), allocatable :: sm
    class(smoother_type), allocatable :: sm2a
  end type
contains
  subroutine save_smoothers(level,save1, save2)
    Implicit None
    type(onelev_type), intent(inout) :: level
    class(smoother_type), allocatable , intent(inout) :: save1, save2
    integer(4) :: info

    info  = 0
! r14-9752 causes the 'stat' declaration from the first ALLOCATE statement
! to disappear, which triggers an ICE in gimplify_var_or_parm_decl. The
! second ALLOCATE statement has to be present for the ICE to occur.
    allocate(save1, mold=level%sm,stat=info)
    allocate(save2, mold=level%sm2a,stat=info)
  end subroutine save_smoothers
end module m
! Two 'stat's from the allocate statements and two from the final wrapper.
! { dg-final { scan-tree-dump-times "integer\\(kind..\\) stat" 4 "original" } }
