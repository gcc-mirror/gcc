! { dg-do compile }
! { dg-options "-fwhole-file -fcoarray=single" }
!
! Procedures with dummy arguments that are coarrays or polymorphic
! must have an explicit interface in the calling routine.
!

MODULE classtype
  type :: t
    integer :: comp
  end type
END MODULE

PROGRAM main
  USE classtype
  CLASS(t), POINTER :: tt

  INTEGER :: coarr[*]

  CALL coarray(coarr)         ! { dg-error " must have an explicit interface" }
  CALL polymorph(tt)          ! { dg-error " must have an explicit interface" }
END PROGRAM

SUBROUTINE coarray(a)
  INTEGER :: a[*]
END SUBROUTINE

SUBROUTINE polymorph(b)
  USE classtype
  CLASS(t) :: b
END SUBROUTINE

! { dg-final { cleanup-modules "classtype" } }
