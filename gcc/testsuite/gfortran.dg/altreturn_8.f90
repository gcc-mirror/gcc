! { dg-do compile }
! { dg-options "-std=gnu" }
!
! PR 56284: [OOP] ICE with alternate return in type-bound procedure
!
! Contributed by Arjen Markus <arjen.markus@deltares.nl>

module try_this
  implicit none

  type :: table_t
  contains
    procedure, nopass :: getRecord
  end type

contains

  subroutine getRecord ( * )
  end subroutine

end module
