! { dg-do compile }
!
! PR fortran/
!
! Contributed by Neil Carlson
!
! Check whether passing an intent(in) pointer
! to an intent(inout) nonpointer is allowed
!
module modA
  type :: typeA
    integer, pointer :: ptr
  end type
contains
  subroutine foo (a,b,c)
    type(typeA), intent(in) :: a
    type(typeA), intent(in) , pointer :: b
    class(typeA), intent(in) , pointer :: c

    call bar (a%ptr)
    call bar2 (b)
    call bar3 (b)
    call bar2 (c)
    call bar3 (c)
    call bar2p (b) ! { dg-error "INTENT\\(IN\\) in pointer association context \\(actual argument to INTENT = OUT/INOUT" }
    call bar3p (b) ! { dg-error "Actual argument to .n. at \\(1\\) must be polymorphic" }
    call bar2p (c) ! { dg-error "INTENT\\(IN\\) in pointer association context \\(actual argument to INTENT = OUT/INOUT" }
    call bar3p (c) ! { dg-error "INTENT\\(IN\\) in pointer association context \\(actual argument to INTENT = OUT/INOUT" }
  end subroutine
  subroutine bar (n)
    integer, intent(inout) :: n
  end subroutine
  subroutine bar2 (n)
    type(typeA), intent(inout) :: n
  end subroutine
  subroutine bar3 (n)
    class(typeA), intent(inout) :: n
  end subroutine
  subroutine bar2p (n)
    type(typeA), intent(inout), pointer :: n
  end subroutine
  subroutine bar3p (n)
    class(typeA), intent(inout), pointer :: n
  end subroutine
end module
