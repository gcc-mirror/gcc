! { dg-do run }
! This tests the fix for PR16940, module interfaces to
! contained functions caused ICEs.
! This is a simplified version of the example in the PR
! discussion, which was due to L.Meissner.
!
! Submitted by Paul Thomas  pault@gcc.gnu.org
!
  module Max_Loc_Mod
    implicit none
    interface Max_Location
      module procedure I_Max_Loc
    end interface
  contains
    function I_Max_Loc (Vector) result(Ans)
      integer, intent (in), dimension(:) :: Vector
      integer, dimension(1) :: Ans
      Ans = maxloc(Vector)
      return
    end function I_Max_Loc
  end module Max_Loc_Mod
  program module_interface
    use Max_Loc_Mod
    implicit none
    integer :: Vector (7)
    Vector = (/1,6,3,5,19,1,2/)
    call Selection_Sort (Vector)
  contains
    subroutine Selection_Sort (Unsorted)
      integer, intent (in), dimension(:) :: Unsorted
      integer, dimension (1) :: N
      N = Max_Location (Unsorted)
      if (N(1).ne.5) call abort ()
      return
    end subroutine Selection_Sort
  end program module_interface

! { dg-final { cleanup-modules "Max_Loc_mod" } }
