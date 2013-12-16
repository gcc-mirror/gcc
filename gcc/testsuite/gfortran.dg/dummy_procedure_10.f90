! { dg-do compile }
!
! PR 35831: [F95] Shape mismatch check missing for dummy procedure argument
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

program test_attributes

  call tester1 (a1)   ! { dg-error "ASYNCHRONOUS mismatch in argument" }
  call tester2 (a2)   ! { dg-error "CONTIGUOUS mismatch in argument" }
  call tester3 (a1)   ! { dg-error "VALUE mismatch in argument" }
  call tester4 (a1)   ! { dg-error "VOLATILE mismatch in argument" }

contains

  subroutine a1(aa)
    real :: aa
  end subroutine
  
  subroutine a2(bb)
    real :: bb(:)
  end subroutine

  subroutine tester1 (f1)
    interface
      subroutine f1 (a)
        real, asynchronous :: a
      end subroutine
    end interface
  end subroutine

  subroutine tester2 (f2)
    interface
      subroutine f2 (b)
        real, contiguous :: b(:)
      end subroutine
    end interface
  end subroutine
  
  subroutine tester3 (f3)
    interface
      subroutine f3 (c)
        real, value :: c
      end subroutine
    end interface
  end subroutine
  
  subroutine tester4 (f4)
    interface
      subroutine f4 (d)
        real, volatile :: d
      end subroutine
    end interface
  end subroutine

end
