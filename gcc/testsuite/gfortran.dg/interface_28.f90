! { dg-do compile }
!
! PR 36947: Attributes not fully checked comparing actual vs dummy procedure
!
! Original test case by Walter Spector <w6ws@earthlink.net>
! Modified by Janus Weil <janus@gcc.gnu.org>

module testsub
  contains
  subroutine test(sub)
    interface
      subroutine sub(x)
        integer, intent(in), optional:: x
      end subroutine
    end interface
    call sub()
  end subroutine
end module

module sub
  contains
  subroutine subActual(x)
    ! actual subroutine's argment is different in intent
    integer, intent(inout),optional:: x
  end subroutine
  subroutine subActual2(x)
    ! actual subroutine's argment is missing OPTIONAL
    integer, intent(in):: x
  end subroutine
end module

program interfaceCheck
  use testsub
  use sub

  integer :: a

  call test(subActual)  ! { dg-error "INTENT mismatch in argument" }
  call test(subActual2)  ! { dg-error "OPTIONAL mismatch in argument" }
end program

! { dg-final { cleanup-modules "sub testsub" } }

