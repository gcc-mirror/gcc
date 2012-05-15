! { dg-do compile }
!
! PR fortran/37445, in which the first version of the fix regressed on the 
! calls to GetBasicElementData; picking up the local GetBasicElementData instead.
!
! Contributed by Norman S Clerman < clerman@fuse.net>
! and reduced by Tobias Burnus <burnus@gcc.gnu.org>
!
MODULE ErrElmnt
  IMPLICIT NONE
  TYPE :: TErrorElement
    integer :: i
  end type TErrorElement
contains
  subroutine GetBasicData ( AnElement, ProcedureName, ErrorNumber,    &
                            Level, Message, ReturnStat)
    type (TErrorElement) :: AnElement
    character (*, 1), optional ::       &
      ProcedureName
    integer (4), optional :: ErrorNumber
    character (*, 1), optional :: Level
    character (*, 1), optional :: Message
    integer (4), optional :: ReturnStat
  end subroutine GetBasicData
end module ErrElmnt

MODULE ErrorMod
  USE ErrElmnt, only: GetBasicElementData => GetBasicData , TErrorElement
  IMPLICIT NONE
contains
  subroutine GetBasicData ()
    integer (4) :: CallingStat, LocalErrorNum
    character (20, 1) :: LocalErrorMessage
    character (20, 1) :: LocalProcName
    character (20, 1) :: Locallevel
    type (TErrorElement) :: AnElement
    call GetBasicElementData (AnElement, LocalProcName, LocalErrorNum, LocalLevel, LocalErrorMessage, CallingStat)
  end subroutine GetBasicData
  SUBROUTINE WH_ERR ()
    integer (4) :: ErrorNumber, CallingStat
    character (20, 1) :: ProcedureName
    character (20, 1) :: ErrorLevel
    character (20, 1) :: ErrorMessage
    type (TErrorElement) :: TargetElement
    call GetBasicElementData (TargetElement, ProcedureName, ErrorNumber, ErrorLevel, ErrorMessage, CallingStat)
  end subroutine WH_ERR
end module ErrorMod
