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


subroutine bar(x,y)
  type myType
    sequence
    integer :: i
  end type myType
  type(myType) :: x
  integer(8) :: y
  if(y /= 8) call abort()
  if(x%i /= 2) call abort()
  x%i = 5
  y   = 42
end subroutine bar

module testmod
  implicit none
  integer, parameter :: kind = 8
  type modType
    real :: rv
  end type modType
  interface
    subroutine other(x,y)
      import ! { dg-error "Fortran 2003: IMPORT statement" }
      type(modType) :: y ! { dg-error "not been declared within the interface" }
      real(kind)    :: x ! { dg-error "has not been declared" }
    end subroutine
  end interface
end module testmod

program foo
  integer, parameter :: dp = 8
  type myType
    sequence
    integer :: i
  end type myType
  type myType3
    sequence
    integer :: i
  end type myType3
  interface
    subroutine bar(x,y)
      import ! { dg-error "Fortran 2003: IMPORT statement" }
      type(myType) :: x ! { dg-error "not been declared within the interface" }
      integer(dp)  :: y ! { dg-error "has not been declared" }
    end subroutine bar
    subroutine test(x)
      import :: myType3 ! { dg-error "Fortran 2003: IMPORT statement" }
      import myType3 ! { dg-error "Fortran 2003: IMPORT statement" }
      type(myType3) :: x ! { dg-error "not been declared within the interface" }
    end subroutine test
  end interface

  type(myType) :: y
  type(myType3) :: z
  integer(dp) :: i8
  y%i = 2
  i8 = 8
  call bar(y,i8) ! { dg-error "Type mismatch in argument" }
  if(y%i /= 5 .or. i8/= 42) call abort()
  z%i = 7
  call test(z) ! { dg-error "Type mismatch in argument" }
  if(z%i /= 1) call abort()
end program foo
! { dg-final { cleanup-modules "testmod" } }
