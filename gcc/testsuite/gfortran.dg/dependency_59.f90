! { dg-do compile }
! PR 95812 - this caused an ICE.
! Test case by Jakub Jelinek.

module test
contains
  subroutine foo()
    integer :: a(3)
    a = 1
    print *, matmul(1*reshape(a,(/3,1/)), reshape((/1,1,1/),(/1,3/)))
  end subroutine foo
  subroutine bar()
    call foo()
  end subroutine bar
end module test
