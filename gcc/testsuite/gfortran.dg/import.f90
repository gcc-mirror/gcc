! { dg-do run }
! Test whether import works
! PR fortran/29601

subroutine test(x)
  type myType3
    sequence
    integer :: i
  end type myType3
  type(myType3) :: x
  if(x%i /= 7) call abort()
  x%i = 1
end subroutine test


subroutine bar(x)
  type myType
    sequence
    integer :: i
  end type myType
  type(myType) :: x
  if(x%i /= 2) call abort()
  x%i = 5
end subroutine bar


program foo
  type myType
    sequence
    integer :: i
  end type myType
  type myType3
    sequence
    integer :: i
  end type myType3
  interface
    subroutine bar(x)
      import
      type(myType) :: x
    end subroutine bar
    subroutine test(x)
      import :: myType3
      import myType3 ! { dg-warning "already IMPORTed from" }
      type(myType3) :: x
    end subroutine test
  end interface

  type(myType) :: y
  type(myType3) :: z
  y%i = 2
  call bar(y)
  if(y%i /= 5) call abort()
  z%i = 7
  call test(z)
  if(z%i /= 1) call abort()
end program foo
