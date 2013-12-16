! { dg-do compile }
!
! PR 54949: [F03] abstract procedure pointers not rejected
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  implicit none

  abstract interface
    subroutine abssub1
    end subroutine
  end interface
  pointer :: abssub1  ! { dg-error "PROCEDURE POINTER attribute conflicts with ABSTRACT attribute" }

  pointer :: abssub2
  abstract interface
    subroutine abssub2  ! { dg-error "PROCEDURE POINTER attribute conflicts with ABSTRACT attribute" }
    end subroutine
  end interface
  
  abssub1 => sub  ! { dg-error "is not a variable" }
  abssub2 => sub
  
contains

  subroutine sub
  end subroutine

end
