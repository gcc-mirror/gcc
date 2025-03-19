! { dg-do run }
!
! Test fix of PR109066, which caused segfaults as below
!
! Contributed by Andrew Benson  <abensonca@gcc.gnu.org>
!
module bugMod

  type :: rm
     integer :: c=0
   contains
     procedure :: rma
     generic   :: assignment(=) => rma
  end type rm

  type :: lc
     type(rm) :: lm
  end type lc

contains

  impure elemental subroutine rma(to,from)
    implicit none
    class(rm), intent(out) :: to
    class(rm), intent(in) :: from
    to%c = -from%c
    return
  end subroutine rma

end module bugMod

program bug
  use bugMod
  implicit none
  type(lc), pointer :: i, j(:)

  allocate (i)
  i = lc (rm (1))                      ! Segmentation fault
  if (i%lm%c .ne. -1) stop 1
  i = i_ptr ()                         ! Segmentation fault
  if (i%lm%c .ne. 1) stop 2

  allocate (j(2))
  j = [lc (rm (2)), lc (rm (3))]       ! Segmentation fault
  if (any (j%lm%c .ne. [-2,-3])) stop 3
  j = j_ptr ()                         ! Worked!
  if (any (j%lm%c .ne. [2,3])) stop 4

contains

  function i_ptr () result(res)
    type(lc), pointer :: res
    res => i
  end function

  function j_ptr () result(res)
    type(lc), pointer :: res (:)
    res => j
  end function

end program bug
