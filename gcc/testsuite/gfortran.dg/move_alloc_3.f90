! { dg-do compile }
!
! PR 44595: INTENT of arguments to intrinsic procedures not checked
!
! Contributed by Janus Weil <janus@gcc.gnu.org>
 
subroutine test(f)
  implicit none
  integer, allocatable, intent(in) :: f
  integer, allocatable :: t
  call move_alloc(f,t)        ! { dg-error "cannot be INTENT.IN." }
end subroutine
