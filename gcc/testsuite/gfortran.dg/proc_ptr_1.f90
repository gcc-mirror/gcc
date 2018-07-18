! { dg-do run }
!
! basic tests of PROCEDURE POINTERS
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m
contains
  subroutine proc1(arg)
    character (5) :: arg
    arg = "proc1"
  end subroutine
  integer function proc2(arg)
    integer, intent(in) :: arg
    proc2 = arg**2
  end function
  complex function proc3(re, im)
    real, intent(in) :: re, im
    proc3 = complex (re, im)
  end function
end module

subroutine foo1
end subroutine

real function foo2()
  foo2=6.3
end function

program procPtrTest
  use m, only: proc1, proc2, proc3
  character (5) :: str
  PROCEDURE(proc1), POINTER :: ptr1
  PROCEDURE(proc2), POINTER :: ptr2
  PROCEDURE(proc3), POINTER :: ptr3 => NULL()
  PROCEDURE(REAL), SAVE, POINTER :: ptr4
  PROCEDURE(), POINTER :: ptr5,ptr6

  EXTERNAL :: foo1,foo2
  real :: foo2

  if(ASSOCIATED(ptr3)) STOP 1

  NULLIFY(ptr1)
  if (ASSOCIATED(ptr1)) STOP 2
  ptr1 => proc1
  if (.not. ASSOCIATED(ptr1)) STOP 3
  call ptr1 (str)
  if (str .ne. "proc1") STOP 4

  ptr2 => NULL()
  if (ASSOCIATED(ptr2)) STOP 5
  ptr2 => proc2
  if (.not. ASSOCIATED(ptr2,proc2)) STOP 6
  if (10*ptr2 (10) .ne. 1000) STOP 7

  ptr3 => NULL (ptr3)
  if (ASSOCIATED(ptr3)) STOP 8
  ptr3 => proc3
  if (ptr3 (1.0, 2.0) .ne. (1.0, 2.0)) STOP 9

  ptr4 => cos
  if (ptr4(0.0)/=1.0) STOP 10

  ptr5 => foo1
  call ptr5()

  ptr6 => foo2
  if (ptr6()/=6.3) STOP 11

end program 
