! { dg-do run }
!
! Tests the fix for PR89363, in which the rank of unallocated or unassociated
! entities, argument associated with assumed rank dummies, was not being set.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_ass_rank_02
  implicit none
contains
  subroutine procr(this,flag)
    real, allocatable :: this(..)
    logical :: flag
    if (rank(this) /= 2 .or. allocated(this)) then
       write(*,*) 'FAIL procr', rank(this), allocated(this)
       flag = .FALSE.
     end if
  end subroutine procr
  subroutine procs(this,flag)
    real, allocatable :: this(..)
    logical :: flag
    if (rank(this) /= 2 .or. .not. allocated(this)) then
       write(*,*) 'FAIL procs status', rank(this), allocated(this)
       flag = .FALSE.
     end if
     if (size(this,1) /= 2 .and. size(this,2) /= 5) then
       write(*,*) 'FAIL procs shape', size(this)
       flag = .FALSE.
     end if
  end subroutine procs
end module mod_ass_rank_02
program ass_rank_02
  use mod_ass_rank_02
  implicit none
  real, allocatable :: x(:,:)
  logical :: flag

  flag = .TRUE.
  call procr(x,flag)
  if (.not.flag) stop 1
  allocate(x(2,5))
  call procs(x,flag)
  if (.not.flag) stop 2
  deallocate(x)
end program ass_rank_02
