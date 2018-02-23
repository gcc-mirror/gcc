! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Tests the fixes for three bugs with the same underlying cause.  All are regressions
! that come about because class array elements end up with a different tree type
! to the class array.  In addition, the language specific flag that marks a class
! container is not being set.
!
! PR53876 contributed by Prince Ogunbade  <pogos77@hotmail.com>
! PR54990 contributed by Janus Weil  <janus@gcc.gnu.org>
! PR54992 contributed by Tobias Burnus  <burnus@gcc.gnu.org>
! The two latter bugs were reported by Andrew Benson
! starting at http://gcc.gnu.org/ml/fortran/2012-10/msg00087.html
!
module G_Nodes
  type :: nc
    type(tn), pointer :: hostNode
  end type nc
  type, extends(nc) :: ncBh
  end type ncBh
  type, public, extends(ncBh) :: ncBhStd
    double precision :: massSeedData
  end type ncBhStd
  type, public :: tn
    class (ncBh), allocatable, dimension(:) :: cBh
  end type tn
  type(ncBhStd) :: defaultBhC
contains
  subroutine Node_C_Bh_Move(targetNode)
    implicit none
    type (tn  ), intent(inout) , target       :: targetNode
    class(ncBh), allocatable   , dimension(:) :: instancesTemporary
! These two lines resulted in the wrong result:
    allocate(instancesTemporary(2),source=defaultBhC)
    call Move_Alloc(instancesTemporary,targetNode%cBh)
! These two lines gave the correct result:
!!deallocate(targetNode%cBh)
!!allocate(targetNode%cBh(2))
    targetNode%cBh(1)%hostNode => targetNode
    targetNode%cBh(2)%hostNode => targetNode
    return
  end subroutine Node_C_Bh_Move
  function bhGet(self,instance)
    implicit none
    class (ncBh), pointer               :: bhGet
    class (tn  ), intent(inout), target :: self
    integer     , intent(in   )         :: instance
    bhGet => self%cBh(instance)
    return
  end function bhGet
end module G_Nodes

  call pr53876
  call pr54990
  call pr54992
end

subroutine pr53876
  IMPLICIT NONE
  TYPE :: individual
    integer :: icomp ! Add an extra component to test offset
    REAL, DIMENSION(:), ALLOCATABLE :: genes
  END TYPE
  CLASS(individual), DIMENSION(:), ALLOCATABLE :: indv
  allocate (indv(2), source = [individual(1, [99,999]), &
                               individual(2, [999,9999])])
  CALL display_indv(indv(2)) ! Similarly, reference 2nd element to test offset
CONTAINS
  SUBROUTINE display_indv(self)
    CLASS(individual),  INTENT(IN) :: self
    if (any(self%genes .ne. [999,9999]) )STOP 1
  END SUBROUTINE
END

subroutine pr54990
  implicit none
  type :: ncBhStd
    integer :: i
  end type
  type, extends(ncBhStd) :: ncBhStde
    integer :: i2(2)
  end type
  type :: tn
    integer :: i ! Add an extra component to test offset
    class (ncBhStd), allocatable, dimension(:) :: cBh
  end type
  integer :: i
  type(tn), target :: a
  allocate (a%cBh(2), source = [(ncBhStde(i*99, [1,2]), i = 1,2)])
  select type (q => a%cBh(2)) ! Similarly, reference 2nd element to test offset
    type is (ncBhStd)
      STOP 2
    type is (ncBhStde)
      if (q%i .ne. 198) STOP 3! This tests that the component really gets the
  end select                       ! language specific flag denoting a class type
end

subroutine pr54992  ! This test remains as the original.
  use G_Nodes
  implicit none
  type (tn), target  :: b
  class(ncBh), pointer :: bh
  class(ncBh), allocatable, dimension(:) :: t
  allocate(b%cBh(1),source=defaultBhC)
  b%cBh(1)%hostNode => b
! #1 this worked
  if (loc(b) .ne. loc(b%cBh(1)%hostNode)) STOP 4
  call Node_C_Bh_Move(b)
! #2 this worked
  if (loc(b) .ne. loc(b%cBh(1)%hostNode)) STOP 5
  if (loc(b) .ne. loc(b%cBh(2)%hostNode)) STOP 6
! #3 this did not
  bh => bhGet(b,instance=1)
  if (loc (b) .ne. loc(bh%hostNode)) STOP 7
  bh => bhGet(b,instance=2)
  if (loc (b) .ne. loc(bh%hostNode)) STOP 8
end
! { dg-final { scan-tree-dump-times "builtin_free" 12 "original" } }
