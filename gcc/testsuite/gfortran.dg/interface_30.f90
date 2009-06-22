! { dg-do compile }
!
! PR39850: Too strict checking for procedures as actual argument
!
! Original test case by Tobias Burnus <burnus@gcc.gnu.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

real function func()
  print *,"func"
  func = 42.0
end function func

program test
  external func1,func2,func3,func4  ! subroutine or implicitly typed real function
  call sub1(func1)
  call sub2(func2)
  call sub1(func3)
  call sub2(func3)  ! { dg-error "is not a subroutine" }
  call sub2(func4)
  call sub1(func4)  ! { dg-error "is not a function" }
contains
  subroutine sub1(a1)
    interface
      real function a1()
      end function
    end interface
    print *, a1()
  end subroutine sub1
  subroutine sub2(a2)
    interface
      subroutine a2
      end subroutine
    end interface
    call a2()
  end subroutine
end

