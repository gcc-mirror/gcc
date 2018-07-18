! { dg-do compile }
!
! PR 60952: [F03] Problem using "end" as a type bound procedure and contained procedures
!
! Contributed by tlcclt <Thomas.L.Clune@nasa.gov>

module A_mod
  implicit none

  type A
  contains
    procedure, nopass :: end
  end type

contains

  subroutine swap
  contains
    subroutine subSwap
    end subroutine
  end subroutine

  integer function end()
  end function

end module
