! { dg-do compile }
! Regression. ICE on valid code.
! The following worked with 4.1.3 and 4.2.2, but failed
! (segmentation fault) with 4.3.0 because the type comparison
! tried to comparethe types of the components of type(node), even
! though the only component is of type(node).
!
! Found using the Fortran Company Fortran 90 Test Suite (Lite),
! Version 1.4
!
! Reported by Tobias Burnus <burnus@gcc.gnu.org>
!
program error
  implicit none
  type node
    sequence
    type(node), pointer :: next
  end type
  type(node), pointer :: list

  interface
    subroutine insert(ptr)
      implicit none
      type node
        sequence
        type(node), pointer :: next
      end type
      type(node), pointer :: ptr
    end subroutine insert
  end interface
  allocate (list);
end program error
