! { dg-do run }
!
! Checks the fix for PR60483.
!
! Contributed by Anthony Lewis  <antony@cosmologist.info>
!
module A
  implicit none
  Type T
    integer :: val = 2
  contains
    final :: testfree
  end type
  integer :: final_flag = 0
contains
  subroutine testfree(this)
    Type(T) this
    final_flag = this%val + final_flag
  end subroutine
  subroutine Testf()
    associate(X => T()) ! This was failing: Symbol 'x' at (1) has no IMPLICIT type
      final_flag = X%val
    end associate
! This should now be 4 but the finalization is not happening.
! TODO put it right!
    if (final_flag .ne. 2) STOP 1
  end subroutine Testf
end module

  use A
  call Testf
end
