! { dg-do run }
! Check that the TYPE_CANONICAL is being correctly set
! for the derived types, when whole file compiling.
! (based on import.f90)
!
subroutine test(x)
  type myType3
    sequence
    integer :: i
  end type myType3
  type(myType3) :: x
  if(x%i /= 7) STOP 1
  x%i = 1
end subroutine test


program foo
  type myType3
    sequence
    integer :: i
  end type myType3

  type(myType3) :: z
  z%i = 7
  call test(z)
  if(z%i /= 1) STOP 1
end program foo
