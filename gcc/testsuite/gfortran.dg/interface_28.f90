! { dg-do compile }
!
! PR 36947: Attributes not fully checked comparing actual vs dummy procedure
!
! Contributed by Walter Spector <w6ws@earthlink.net>

module testsub
  contains
  subroutine test(sub)
    interface
      subroutine sub(x)
        integer, intent(in), optional:: x
      end subroutine
    end interface
    print *, "In test(), about to call sub()"
    call sub()
  end subroutine
end module

module sub
  contains
  subroutine subActual(x)
    ! actual subroutine's argment is different in intent and optional
    integer, intent(inout):: x
    print *, "In subActual():", x
  end subroutine
end module

program interfaceCheck
  use testsub
  use sub

  integer :: a

  call test(subActual)  ! { dg-error "Type/rank mismatch in argument" }
end program

! { dg-final { cleanup-modules "sub testsub" } }

