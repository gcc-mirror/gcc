! { dg-do compile }
!
! PR 35831: [F95] Shape mismatch check missing for dummy procedure argument
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

call call_a(a1)  ! { dg-error "Character length mismatch in function result" }
call call_b(b1)  ! { dg-error "Shape mismatch" }
call call_c(c1)  ! { dg-error "POINTER attribute mismatch in function result" }
call call_d(c1)  ! { dg-error "ALLOCATABLE attribute mismatch in function result" }
call call_e(e1)  ! { dg-error "CONTIGUOUS attribute mismatch in function result" }
call call_f(c1)  ! { dg-error "PROCEDURE POINTER mismatch in function result" }

contains

  character(1) function a1()
  end function

  subroutine call_a(a3)
    interface
      character(2) function a3()
      end function
    end interface
  end subroutine


  function b1()
    integer, dimension(1:3) :: b1
  end function

  subroutine call_b(b2)
    interface
      function b2()
        integer, dimension(0:4) :: b2
      end function
    end interface
  end subroutine


  integer function c1()
  end function

  subroutine call_c(c2)
    interface
      function c2()
        integer, pointer :: c2
      end function
    end interface
  end subroutine


  subroutine call_d(d2)
    interface
      function d2()
        integer, allocatable :: d2
      end function
    end interface
  end subroutine


  function e1()
    integer, dimension(:), pointer :: e1
  end function

  subroutine call_e(e2)
    interface
      function e2()
        integer, dimension(:), pointer, contiguous :: e2
      end function
    end interface
  end subroutine


  subroutine call_f(f2)
    interface
      function f2()
        procedure(integer), pointer :: f2
      end function
    end interface
  end subroutine

end
