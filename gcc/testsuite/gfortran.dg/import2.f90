! { dg-do compile }
! { dg-options "-std=f95" }
! { dg-shouldfail "Fortran 2003 feature with -std=f95" }
! Test whether import does not work with -std=f95
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
      import ! { dg-error "Fortran 2003: IMPORT statement" }
      type(myType) :: x ! { dg-error "not been declared within the interface" }
    end subroutine bar
    subroutine test(x)
      import :: myType3 ! { dg-error "Fortran 2003: IMPORT statement" }
      import myType3 ! { dg-error "Fortran 2003: IMPORT statement" }
      type(myType3) :: x ! { dg-error "not been declared within the interface" }
    end subroutine test
  end interface

  type(myType) :: y
  type(myType3) :: z
  y%i = 2
  call bar(y) ! { dg-error "Type/rank mismatch in argument" }
  if(y%i /= 5) call abort()
  z%i = 7
  call test(z) ! { dg-error "Type/rank mismatch in argument" }
  if(z%i /= 1) call abort()
end program foo
